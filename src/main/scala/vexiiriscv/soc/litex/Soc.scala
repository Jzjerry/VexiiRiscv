package vexiiriscv.soc.litex

import spinal.core.fiber.Fiber
import spinal.core._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4SpecRenamer, Axi4ToTilelinkFiber}
import spinal.lib.bus.amba4.axilite.AxiLite4SpecRenamer
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink.coherent.{CacheFiber, HubFiber}
import spinal.lib.bus.tilelink.fabric
import spinal.lib.bus.tilelink.fabric.Node
import spinal.lib.cpu.riscv.debug.DebugModuleFiber
import spinal.lib.misc.{PathTracer, TilelinkClintFiber}
import spinal.lib.misc.plic.TilelinkPlicFiber
import spinal.lib.{AnalysisUtils, Delay, Flow, ResetCtrlFiber, StreamPipe, master, slave}
import spinal.lib.system.tag.{MemoryConnection, MemoryEndpoint, MemoryTransferTag, PMA}
import vexiiriscv.ParamSimple
import vexiiriscv.compat.{EnforceSyncRamPhase, MultiPortWritesSymplifier}
import vexiiriscv.prediction.GSharePlugin
import vexiiriscv.soc.TilelinkVexiiRiscvFiber
import vexiiriscv.soc.demo.DebugModuleSocFiber

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class LitexMemoryRegion(mapping : SizeMapping, mode : String, bus : String){
  def isExecutable = mode.contains("x")
  def isCachable = mode.contains("c")
  def onPeripheral = bus match {
    case "m" => false
    case "p" => true
  }
  def onMemory = !onPeripheral
}

class SocConfig(){
  var vexiiParam = new ParamSimple()
  val regions = ArrayBuffer[LitexMemoryRegion]()
  var withJtagTap = false
  var withJtagInstruction = false
  def withDebug = withJtagInstruction || withJtagTap
  var withDma = false
  var mBusWidth = 64
  var l2Bytes = 0
  var l2Ways = 0
  var cpuCount = 1
  var litedramWidth = 32
//  var sharedBusWidth = 32
  def withL2 = l2Bytes > 0
}

class Soc(c : SocConfig, systemCd : ClockDomain) extends Component{
  import c._

  val system = systemCd on new AreaRoot {
    val mainDataWidth = vexiiParam.memDataWidth
    val withCoherency = vexiiParam.lsuL1Coherency
    val vexiis = for (hartId <- 0 until cpuCount) yield new TilelinkVexiiRiscvFiber(vexiiParam.plugins(hartId))
    for (vexii <- vexiis) {
      vexii.lsuL1Bus.setDownConnection(a = StreamPipe.HALF, b = StreamPipe.HALF, c = StreamPipe.FULL, d = StreamPipe.M2S, e = StreamPipe.HALF)
      vexii.dBus.setDownConnection(a = StreamPipe.HALF, d = StreamPipe.M2S)
      vexii.iBus.setDownConnection(a = StreamPipe.HALF, d = StreamPipe.M2S)
    }

    val cBus, ioBus = fabric.Node()
    ioBus.setUpConnection(a = StreamPipe.HALF, d = StreamPipe.NONE)
    for (vexii <- vexiis) {
      cBus << List(vexii.iBus, vexiiParam.fetchL1Enable.mux(vexii.lsuL1Bus, vexii.dBus))
      if(vexiiParam.fetchL1Enable) ioBus << List(vexii.dBus)
    }

    val dma = c.withDma generate new Area {
      val bus = slave(
        Axi4(
          Axi4Config(
            addressWidth = 32,
            dataWidth = mainDataWidth,
            idWidth = 4
          )
        )
      )

      val bridge = new Axi4ToTilelinkFiber(64, 4)
      bridge.up load bus.pipelined(ar = StreamPipe.HALF, aw = StreamPipe.HALF, w = StreamPipe.FULL, b = StreamPipe.HALF, r = StreamPipe.FULL)

      val filter = new fabric.TransferFilter()
      filter.up << bridge.down
      cBus << filter.down
      filter.down.setDownConnection(a = StreamPipe.FULL)



      //As litex reset will release before our one, we need to ensure that we don't eat a transaction
      Fiber build {
        bridge.read.get
        bridge.write.get
        when(ClockDomain.current.isResetActive){
          bus.ar.ready := False
          bus.aw.ready := False
          bus.w.ready := False
        }
      }
    }

    assert(!(!withCoherency && withL2))

    var perfBus: Node = null
    val direct = (!withCoherency) generate new Area{
      perfBus = cBus
    }

    val hub = (withCoherency && !withL2) generate new Area {
      val hub = new HubFiber()
      hub.up << cBus
      hub.up.setUpConnection(a = StreamPipe.FULL, c = StreamPipe.FULL)
      hub.down.forceDataWidth(mainDataWidth)
      perfBus = hub.down
    }

    val l2 = (withCoherency && withL2) generate new Area {
      val cache = new CacheFiber()
      cache.parameter.cacheWays = l2Ways
      cache.parameter.cacheBytes = l2Bytes
      cache.up << cBus
      cache.up.setUpConnection(a = StreamPipe.FULL, c = StreamPipe.FULL, d = StreamPipe.FULL)
      cache.down.setDownConnection(d = StreamPipe.S2M)
      cache.down.forceDataWidth(mainDataWidth)
      perfBus = cache.down
    }

    val memRegions = regions.filter(e => e.onMemory && e.isCachable)
    val axiLiteRegions = regions.filter(e => e.onPeripheral)

    val withMem = memRegions.nonEmpty
    val toAxi4 = withMem generate new fabric.Axi4Bridge
    if (withMem) {
      toAxi4.up.forceDataWidth(litedramWidth)
      regions.filter(_.onMemory).foreach(r =>
        toAxi4.up at r.mapping of perfBus
      )
      toAxi4.down.addTag(PMA.MAIN)
      toAxi4.down.addTag(PMA.EXECUTABLE)
      for(region <- memRegions) {
        toAxi4.down.addTag(new MemoryEndpoint {
          override def mapping = SizeMapping(0, region.mapping.size)
        })
      }
    }


    val peripheral = new Area {
      val bus = Node()
      bus << (perfBus, ioBus)
      bus.setDownConnection(a = StreamPipe.HALF, d = StreamPipe.HALF)
      bus.forceDataWidth(32)

      val clint = new TilelinkClintFiber()
      clint.node at 0xF0010000l of bus

      val plic = new TilelinkPlicFiber()
      plic.node at 0xF0C00000l of bus

      val externalInterrupts = new Area {
        val port = in Bits (32 bits)
        val toPlic = for (i <- 0 to 31) yield (i != 0) generate new Area {
          val node = plic.createInterruptSlave(i)
          node.withUps = false
          node.flag := port(i)
        }
      }


      for (vexii <- vexiis) {
        vexii.bind(clint)
        vexii.bind(plic)
      }

      val toAxiLite4 = new fabric.AxiLite4Bridge
      toAxiLite4.up << bus




      val virtualRegions = for (region <- axiLiteRegions) yield new Area with SpinalTagReady {
        def self = this

        new MemoryConnection {
          override def up = toAxiLite4.down
          override def down = self
          override def transformers = Nil
//          override def mapping = region.mapping //TODO
          populate()
        }
        self.addTag(new MemoryEndpoint {
          override def mapping = region.mapping
        })

        addTag(new MemoryTransferTag {
          override def get = toAxiLite4.up.m2s.parameters.emits
        })
        if (region.isCachable) addTag(PMA.MAIN)
        if (region.isExecutable) addTag(PMA.EXECUTABLE)
      }
    }

    val mBus = withMem generate (Fiber build master(toAxi4.down.pipelined()))
    val pBus = Fiber build master(peripheral.toAxiLite4.down.pipelined(ar = StreamPipe.HALF, aw = StreamPipe.HALF, w = StreamPipe.HALF, b = StreamPipe.HALF, r = StreamPipe.HALF))

//    val debug = c.withDebug generate new Area {
//      val cd = ClockDomain.current.copy(reset = in Bool())
//      val cdi = c.withJtagInstruction generate ClockDomain.external("jtag_instruction", withReset = false)
//
//      val dm = cd(new DebugModuleFiber())
//      vexiis.foreach(dm.bindHart)
//      val tap = c.withJtagTap generate cd(dm.withJtagTap())
//      val instruction = c.withJtagInstruction generate cdi(dm.withJtagInstruction())
//    }

    val patcher = Fiber build new Area {
      if (c.withDma) {
        Axi4SpecRenamer(dma.bus)
      }
      if (withMem) Axi4SpecRenamer(mBus.get)
      AxiLite4SpecRenamer(pBus.get)

//      vexii(0).dBus.bus


      val i = MemoryConnection.getMemoryTransfers(vexiis(0).iBus)
      val d = MemoryConnection.getMemoryTransfers(vexiis(0).dBus)
//      val p = MemoryConnection.getMemoryTransfers(vexiis(0).pBus)

      println(i)

//      if (withJtagTap) debug.tap.jtag.setName("jtag")
//      if (withJtagInstruction) debug.instruction.setName("jtag_instruction")
//      if (c.withDebug) {
//        debug.dm.ndmreset.toIo().setName("debug_ndmreset")
//        debug.cd.reset.setName("debug_reset")
//      }

//      val tracer = master(Reg(Flow(Bits(8 bits))))
//      val trigger = False
//      tracer.valid init (False)
//      tracer.valid := Delay(trigger, 2)
//      tracer.payload init (0)
//      for (nax <- vexii) {
//        nax.plugins.collectFirst { case p: CsrTracer => p } match {
//          case Some(p) => when(p.logic.flowOut.valid) {
//            trigger := True
//            tracer.payload := p.logic.flowOut.payload
//          }
//          case None =>
//        }
//      }

//      vexiis.foreach(_.logic.core.addAttribute("keep_hierarchy", "yes"))
//      component.definition.addAttribute("keep_hierarchy", "yes")
    }
  }

  val debugReset = c.withDebug generate in.Bool()
  val debug = c.withDebug generate ClockDomain(systemCd.clock, debugReset)(new DebugModuleSocFiber(withJtagInstruction) {
    out(dm.ndmreset)
    system.vexiis.foreach(bindHart)
  })
}




object SocGen extends App{
  var netlistDirectory = "."
  var netlistName = "VexiiRiscvLitex"
  val socConfig = new SocConfig()
  import socConfig._

  vexiiParam.fetchL1Enable = true
  vexiiParam.lsuL1Enable = true
  vexiiParam.privParam.withRdTime = true

  assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("prints this usage text")
    vexiiParam.addOptions(this)
    opt[String]("netlist-directory") action { (v, c) => netlistDirectory = v }
    opt[String]("netlist-name") action { (v, c) => netlistName = v }
    opt[Int]("litedram-width") action { (v, c) => litedramWidth = v }
    opt[Int]("cpu-count") action { (v, c) => cpuCount = v }
    opt[Int]("l2-bytes") action { (v, c) => l2Bytes = v }
    opt[Int]("l2-ways") action { (v, c) => l2Ways = v }
    opt[Unit]("with-dma") action { (v, c) => withDma = true }
    opt[Unit]("with-jtag-tap") action { (v, c) => withJtagTap = true }
    opt[Unit]("with-jtag-instruction") action { (v, c) => withJtagInstruction = true }
    opt[Seq[String]]("memory-region") unbounded() action  { (v, c) =>
      assert(v.length == 4, "--memory-region need 4 parameters")
      val r = new LitexMemoryRegion(SizeMapping(BigInt(v(0)), BigInt(v(1))), v(2), v(3))
      regions += r
      assert(!(r.onMemory && !r.isCachable), s"Region $r isn't supported by VexiiRiscv, data cache will always cache memory")
    }
  }.parse(args, Unit).nonEmpty)

  vexiiParam.lsuL1Coherency = cpuCount > 1 || withDma

  val spinalConfig = SpinalConfig(inlineRom = true, targetDirectory = netlistDirectory)
  spinalConfig.addTransformationPhase(new MultiPortWritesSymplifier)
  spinalConfig.addStandardMemBlackboxing(blackboxByteEnables)
  spinalConfig.addTransformationPhase(new EnforceSyncRamPhase)

  val report = spinalConfig.generateVerilog {
    new Soc(socConfig, ClockDomain.external("system")).setDefinitionName(netlistName)
  }

  val cpu0 = report.toplevel.system.vexiis(0).logic.core
//  val from = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_execute_ctrl3_up_LsuL1_PHYSICAL_ADDRESS_lane0")
//  val to = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_execute_ctrl1_up_float_RS1_lane0")

//  val from = cpu0.reflectBaseType("DispatchPlugin_logic_slots_0_ctx_uop")
//  val from = cpu0.reflectBaseType("DispatchPlugin_logic_slots_0_ctx_hm_RS3_PHYS")
//  val to = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_execute_ctrl0_down_float_RS3_lane0")

//  val from = cpu0.reflectBaseType("LsuL1Plugin_logic_c_pip_ctrl_2_up_LsuL1_PHYSICAL_ADDRESS") // start point was optimized, but aligner timing issue remains
//  val to = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_decode_ctrls_1_up_Decode_INSTRUCTION_0")

//  val from = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_execute_ctrl1_up_Decode_UOP_lane0")
//  val to = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_decode_ctrls_1_up_Decode_INSTRUCTION_0")

//  val from = cpu0.reflectBaseType("fetch_logic_ctrls_2_down_valid")
//  val to = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_decode_ctrls_1_up_Decode_INSTRUCTION_0")

//  val from = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_execute_ctrl3_up_LsuL1_PHYSICAL_ADDRESS_lane0")
//  val to = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_execute_ctrl1_up_integer_RS2_lane0")

//  val from = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_execute_ctrl1_up_float_RS1_lane0")
//  val to = cpu0.reflectBaseType("FpuPackerPlugin_logic_pip_node_1_s0_EXP_DIF_PLUS_ONE")

//  val from = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_decode_ctrls_1_up_Decode_INSTRUCTION_0")
//  val to = cpu0.host[GSharePlugin].logic.onLearn.cmd.valid

  val from = cpu0.reflectBaseType("vexiis_0_logic_core_toplevel_execute_ctrl1_up_float_RS1_lane0")
  val to = cpu0.reflectBaseType("LsuL1Plugin_logic_writeback_slots_1_timer_counter")

  val drivers = mutable.LinkedHashSet[BaseType]()
  AnalysisUtils.seekNonCombDrivers(to){driver =>
    driver match {
      case bt : BaseType => drivers += bt
    }
  }
  drivers.foreach(e => println(e.getName()))
  println("******")
  println(PathTracer.impl(from, to).report())
}

object PythonArgsGen extends App{
  val vexiiParam = new ParamSimple()
  import vexiiParam._
  var pythonPath ="miaou.py"
  assert(new scopt.OptionParser[Unit]("Vexii") {
    help("help").text("prints this usage text")
    vexiiParam.addOptions(this)
    opt[String]("python-file") action { (v, c) => pythonPath = v }

  }.parse(args, Unit).nonEmpty)

  import java.io.PrintWriter

  new PrintWriter(pythonPath) {
    write(
      s"""
         |VexiiRiscv.xlen = $xlen
         |VexiiRiscv.with_rvc = ${withRvc.toInt}
         |VexiiRiscv.with_rvm = ${(withMul && withDiv).toInt}
         |VexiiRiscv.internal_bus_width = ${memDataWidth}
         |""".stripMargin)
    close()
  }

}

/*
vex 1 =>
Memspeed at 0x40000000 (Sequential, 8.0KiB)...
  Write speed: 1.6MiB/s
   Read speed: 867.6KiB/s


Write speed: 647.4KiB/s
 Read speed: 689.3KiB/s

Write speed: 811.9KiB/s
 Read speed: 833.5KiB/s

Write speed: 1.3MiB/s
 Read speed: 833.5KiB/s

Write speed: 1.3MiB/s
 Read speed: 1.0MiB/s


python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --vexii-args="--allow-bypass-from=0 --debug-privileged" --with-jtag-tap --build --load
--with-video-framebuffer --with-spi-sdcard --with-ethernet

litex_sim --cpu-type=vexiiriscv  --with-sdram --sdram-data-width=64 --bus-standard axi-lite --vexii-args="--allow-bypass-from=0 --with-mul --with-div --with-rva --with-btb --with-ras --with-gshare --fetch-l1 --fetch-l1-sets=64 --fetch-l1-ways=4 --lsu-l1 --lsu-l1-sets=64 --lsu-l1-ways=4 --lsu-l1-store-buffer-slots=2 --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-supervisor --with-user --performance-counters 0 --debug-privileged --debug-triggers=4 --fetch-l1-mem-data-width-min=64 --lsu-l1-mem-data-width-min=64" --with-jtag-tap --trace-fst --with-jtagremote


litex_sim --cpu-type=vexiiriscv  --with-sdram --sdram-data-width=64 --bus-standard axi-lite --vexii-args="--allow-bypass-from=0 --with-mul --with-div --with-rva --with-btb --with-ras --with-gshare --fetch-l1-sets=64 --fetch-l1-ways=4 --lsu-l1-sets=64 --lsu-l1-ways=4 --lsu-l1-store-buffer-slots=2 --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-supervisor --with-user --performance-counters 0" --trace-fst --sdram-init /media/data2/proj/vexii/litex/buildroot/rv32ima/images/boot.json --trace-start 120000000000000 --trace-end 122000000000000 --trace

litex_sim --cpu-type=vexiiriscv  --with-sdram --sdram-data-width=64 --bus-standard axi-lite --vexii-args="--allow-bypass-from=0 --with-mul --with-div --with-rva --with-btb --with-ras --with-gshare --fetch-l1-sets=64 --fetch-l1-ways=4 --lsu-l1-sets=64 --lsu-l1-ways=4 --lsu-l1-store-buffer-slots=2 --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-supervisor --with-user --performance-counters 0" --trace-fst --sdram-init /media/data2/proj/vexii/litex/buildroot/rv32ima/images/boot.json --trace-start 120000000000000 --trace-end 122000000000000 --trace

litex_sim --cpu-type=vexiiriscv  --with-sdram --sdram-data-width=64  \
--vexii-args=" \
--allow-bypass-from=0 --debug-privileged --with-mul --with-div --div-ipc --with-rva --with-supervisor --performance-counters 0 \
--regfile-async --xlen=64 --with-rvc --with-rvf --with-rvd \
--fetch-l1 --fetch-l1-ways=4 --fetch-l1-mem-data-width-min=64 \
--lsu-l1 --lsu-l1-ways=4  --lsu-l1-mem-data-width-min=64 --lsu-l1-refill-count 1 --lsu-l1-writeback-count 1 --with-lsu-bypass \
--relaxed-branch"  --cpu-count=2 --with-jtag-tap  --with-jtagremote  --sdram-init boot.json

--sdram-init /media/data2/proj/vexii/litex/buildroot/rv32ima/images/boot.json

--decoders=2 --lanes=2 --with-dispatcher-buffer"

--trace
--trace-start 600000000000
60000000000000
--sdram-init images/sim.json


/media/data2/proj/upstream/openocd_riscv_up/src/openocd -f ft2232h_breakout.cfg -f vexiiriscv_jtag.tcl -c "load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images/rootfs.cpio 0x40000000" -c exit
(* MARK_DEBUG = "TRUE" *)

// Minimal linux
python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --vexii-args="--allow-bypass-from=0 --debug-privileged --with-mul --with-div --with-rva --with-supervisor --performance-counters 0" --with-jtag-tap  --load
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images/Image 0x40000000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images/rv32.dtb 0x40ef0000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images/rootfs.cpio 0x41000000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images/opensbi.bin 0x40f00000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/opensbi/build/platform/litex/vexriscv/firmware/fw_jump.bin 0x40f00000
resume

//Linux++ single issue 1.77
python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --bus-standard axi-lite --vexii-args="--allow-bypass-from=0 --debug-privileged --with-mul --with-div --div-ipc --with-rva --with-supervisor --performance-counters 0 --fetch-l1 --fetch-l1-ways=4 --lsu-l1 --lsu-l1-ways=4 --fetch-l1-mem-data-width-min=64 --lsu-l1-mem-data-width-min=64  --with-btb --with-ras --with-gshare --relaxed-branch --regfile-async --lsu-l1-store-buffer-slots=2 --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-lsu-bypass" --with-jtag-tap  --build --load

//Linux++ dual issue
python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --bus-standard axi-lite --vexii-args="--allow-bypass-from=0 --debug-privileged --with-mul --with-div --div-ipc --with-rva --with-supervisor --performance-counters 0 --fetch-l1 --fetch-l1-ways=4 --lsu-l1 --lsu-l1-ways=4 --fetch-l1-mem-data-width-min=64 --lsu-l1-mem-data-width-min=64  --with-btb --with-ras --with-gshare --relaxed-branch --regfile-async --lsu-l1-store-buffer-slots=2 --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-lsu-bypass --decoders=2 --lanes=2" --with-jtag-tap  --build --load

//linux++ 64 bits fpu
python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --with-jtag-tap  --bus-standard axi-lite --vexii-args=" \
--allow-bypass-from=0 --debug-privileged --with-mul --with-div --div-ipc --with-rva --with-supervisor --performance-counters 0 \
--regfile-async --xlen=64 --with-rvc --with-rvf --with-rvd --fma-reduced-accuracy \
--fetch-l1 --fetch-l1-ways=4 --fetch-l1-mem-data-width-min=64 \
--lsu-l1 --lsu-l1-ways=4  --lsu-l1-mem-data-width-min=64 --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --lsu-l1-store-buffer-slots=2  --with-lsu-bypass \
--with-btb --with-ras --with-gshare --relaxed-branch"  --cpu-count=2 --with-jtag-tap  --with-video-framebuffer --with-sdcard --with-ethernet --with-coherent-dma --l2-byte=131072 --update-repo=no  --sys-clk-freq 100000000 --build   --load



python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --vexii-args="--debug-privileged" --with-jtag-tap --build --load
export HART_COUNT=2
/media/data2/proj/upstream/openocd_riscv_up/src/openocd -f ft2232h_breakout.cfg -f vexiiriscv_jtag.tcl

python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --vexii-args="--debug-privileged" --with-jtag-instruction --build --load
openocd -f digilent_nexys_video.tcl -f vexiiriscv_jtag_tunneled.tcl


python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --vexii-args="--allow-bypass-from=0 --debug-privileged --with-mul --with-div --with-rva --with-supervisor --performance-counters 0 --fetch-l1 --fetch-l1-ways=4 --lsu-l1 --lsu-l1-ways=4  --with-btb --with-ras --with-gshare" --with-jtag-tap  --load



opensbi =>
git clone https://github.com/Dolu1990/opensbi.git --branch uart-fix
cd opensbi
make PLATFORM_RISCV_XLEN=64 PLATFORM_RISCV_ABI=lp64d PLATFORM_RISCV_ISA=rv64gc CROSS_COMPILE=riscv-none-embed- PLATFORM=litex/vexriscv

//linux ++ dual core
make O=build/full  BR2_EXTERNAL=../config litex_vexriscv_full_defconfig
(cd build/full/ && make -j20)

litex_sim --cpu-type=vexiiriscv  --with-sdram --sdram-data-width=64 --bus-standard axi-lite --vexii-args="--allow-bypass-from=0 --debug-privileged --with-mul --with-div --div-ipc --with-rva --with-supervisor --performance-counters 0 --fetch-l1 --fetch-l1-ways=4 --lsu-l1 --lsu-l1-ways=4 --fetch-l1-mem-data-width-min=64 --lsu-l1-mem-data-width-min=64  --with-btb --with-ras --with-gshare --relaxed-branch --regfile-async --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-lsu-bypass" --cpu-count=2  --with-jtag-tap --sdram-init /media/data2/proj/vexii/litex/buildroot/rv32ima/images/boot.json
python3 -m litex_boards.targets.digilent_nexys_video --soc-json build/digilent_nexys_video/csr.json --cpu-type=vexiiriscv  --vexii-args="--allow-bypass-from=0 --debug-privileged --with-mul --with-div --div-ipc --with-rva --with-supervisor --performance-counters 0 --fetch-l1 --fetch-l1-ways=4 --lsu-l1 --lsu-l1-ways=4 --fetch-l1-mem-data-width-min=64 --lsu-l1-mem-data-width-min=64  --with-btb --with-ras --with-gshare --relaxed-branch --regfile-async --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-lsu-bypass" --cpu-count=2 --with-jtag-tap  --with-video-framebuffer --with-spi-sdcard --with-ethernet  --build --load
python3 -m litex_boards.targets.digilent_nexys_video --soc-json build/digilent_nexys_video/csr.json --cpu-type=vexiiriscv  --vexii-args="--allow-bypass-from=0 --debug-privileged --with-mul --with-div --div-ipc --with-rva --with-supervisor --performance-counters 0 --fetch-l1 --fetch-l1-ways=4 --lsu-l1 --lsu-l1-ways=4 --fetch-l1-mem-data-width-min=64 --lsu-l1-mem-data-width-min=64  --with-btb --with-ras --with-gshare --relaxed-branch --regfile-async --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --with-lsu-bypass" --cpu-count=2 --with-jtag-tap  --with-video-framebuffer --with-sdcard --with-ethernet --with-coherent-dma --l2-bytes=131072 --load
--lsu-l1-store-buffer-slots=2 --lsu-l1-store-buffer-ops=32

export HART_COUNT=2
/media/data2/proj/upstream/openocd_riscv_up/src/openocd -f ft2232h_breakout.cfg -f vexiiriscv_jtag.tcl -f dev.tcl

load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images_full/Image 0x40000000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images_full/linux_2c.dtb 0x40ef0000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/images_full/rootfs.cpio 0x41000000
load_image /media/data2/proj/vexii/litex/buildroot/rv32ima/opensbi/build/platform/litex/vexriscv/firmware/fw_jump.bin 0x40f00000
targets riscv.cpu.1; resume
targets riscv.cpu.0; resume

boot 0x40f00000

udhcpc
cat >> /etc/X11/xorg.conf << EOF
> Section "Module"
>   Load "fb"
>   Load "shadow"
>   Load "fbdevhw"
> EndSection
> EOF




TODO debug :
[ 9576.106084] CPU: 0 PID: 4072 Comm: gmain Not tainted 6.1.0-rc2+ #11
[ 9576.109440] watchdog: BUG: soft lockup - CPU#1 stuck for 22s! [gdbus:4073]
[ 9576.111128] epc : find_vma+0x14/0x44
[ 9576.116689] CPU: 1 PID: 4073 Comm: gdbus Not tainted 6.1.0-rc2+ #11
[ 9576.119598]  ra : do_page_fault+0xf2/0x31a
[ 9576.124672] epc : handle_mm_fault+0x3c/0xd6
[ 9576.128004] epc : ffffffff8010b100 ra : ffffffff800073d8 sp : ffffffc800ecb880
[ 9576.131406]  ra : handle_mm_fault+0x36/0xd6
[ 9576.137241]  gp : ffffffff810c9240 tp : ffffffd802923480 t0 : ffffffff800072e6
[ 9576.140644] epc : ffffffff8010580a ra : ffffffff80105804 sp : ffffffc800ed3870
[ 9576.146478]  t1 : ffffffff80c00208 t2 : ffffffff80c00288 s0 : ffffffc800ecb8a0
[ 9576.152313]  gp : ffffffff810c9240 tp : ffffffd80435b480 t0 : 0000000000000001
[ 9576.158148]  s1 : ffffffc800ecb920 a0 : ffffffd8008f0f00 a1 : 0000002ac1f46b36
[ 9576.163982]  t1 : 000000000000000e t2 : 0000000000000002 s0 : ffffffc800ed38a0
[ 9576.169818]  a2 : ffffffff810c8a68 a3 : 0000000000040000 a4 : 0000000000000000
[ 9576.175651]  s1 : 0000000000000215 a0 : 0000000000000400 a1 : 0000000000000002
[ 9576.181486]  a5 : 0000002ac1f46b36 a6 : ffffffc800ecba88 a7 : 0000000000000002
[ 9576.187321]  a2 : 0000000000000008 a3 : 0000000000000007 a4 : 0000000000000000
[ 9576.193156]  s2 : ffffffd8008f0f00 s3 : ffffffd802923480 s4 : 0000002ac1f46b36
[ 9576.198990]  a5 : 0000000000001000 a6 : 0000000000001000 a7 : 00000000000000fd
[ 9576.204825]  s5 : ffffffd802923480 s6 : 0000000000000215 s7 : 000000000000000c
[ 9576.210659]  s2 : 0000000000000400 s3 : 0000003f94000b96 s4 : ffffffc800ed3920
[ 9576.216495]  s8 : 000000000000000f s9 : 000000000000000d s10: ffffffd8008f0f60
[ 9576.222329]  s5 : ffffffd8008442a8 s6 : 0000000000000215 s7 : 000000000000000c
[ 9576.228164]  s11: 000000000000000f t3 : 0000000000000001 t4 : 0000000000000340
[ 9576.233998]  s8 : 000000000000000f s9 : 000000000000000d s10: ffffffd8008f0f60
[ 9576.239833]  t5 : 0000000000000000 t6 : 0000000000000000
[ 9576.245667]  s11: 000000000000000f t3 : 0000003f94000000 t4 : ffffffd803d5e10c
[ 9576.249974] status: 0000000200000120 badaddr: 0000000000000000 cause: 8000000000000005
[ 9576.255808]  t5 : 0000000000000000 t6 : 0000003fa3b98fff
[ 9576.262205] [<ffffffff800073d8>] do_page_fault+0xf2/0x31a
[ 9576.266506] status: 0000000200000120 badaddr: 0000000000000000 cause: 8000000000000005
[ 9576.270884] [<ffffffff80002f76>] ret_from_exception+0x0/0xc
[ 9576.277276] [<ffffffff80007402>] do_page_fault+0x11c/0x31a
[ 9576.281789] [<ffffffff80144a98>] do_sys_poll+0x144/0x42c
[ 9576.286235] [<ffffffff80002f76>] ret_from_exception+0x0/0xc
[ 9576.295073] [<ffffffff80144a98>] do_sys_poll+0x144/0x42c



debug fmax =>
python3 -m litex_boards.targets.digilent_nexys_video --cpu-type=vexiiriscv  --with-jtag-tap  --bus-standard axi-lite --vexii-args=" \
--allow-bypass-from=0 --debug-privileged --with-mul --with-div --div-ipc --with-rva --with-supervisor --performance-counters 0 \
--regfile-async --xlen=64 --with-rvc --with-rvf --with-rvd --decoders=2 --lanes=2 --with-dispatcher-buffer \
--fetch-l1 --fetch-l1-ways=4 --fetch-l1-mem-data-width-min=64 \
--lsu-l1 --lsu-l1-ways=4  --lsu-l1-mem-data-width-min=64 --lsu-l1-store-buffer-ops=32 --lsu-l1-refill-count 2 --lsu-l1-writeback-count 2 --lsu-l1-store-buffer-slots=2  --with-lsu-bypass \
--with-btb --with-ras --with-gshare --relaxed-branch"  --cpu-count=2 --with-jtag-tap  --with-video-framebuffer --with-sdcard --with-ethernet --with-coherent-dma --l2-byte=131072 --update-repo=no  --sys-clk-freq 80000000    --build  --sys-clk-freq 80000000

- Node((toplevel/vexiis_0_logic_core/vexiis_0_logic_core_toplevel_decode_ctrls_1_up_Decode_INSTRUCTION_0 :  Bits[32 bits]))
  - Node((toplevel/vexiis_0_logic_core/vexiis_0_logic_core_toplevel_decode_ctrls_0_down_Decode_INSTRUCTION_0 :  Bits[32 bits]))
    - Node((toplevel/vexiis_0_logic_core/vexiis_0_logic_core_toplevel_decode_ctrls_0_up_Decode_INSTRUCTION_0 :  Bits[32 bits]))
      - Node((toplevel/vexiis_0_logic_core/AlignerPlugin_logic_extractors_0_ctx_instruction :  Bits[32 bits]))
        - Node((Bits | Bits)[32 bits])
          - Node((Bits | Bits)[32 bits])
            - Node((Bits | Bits)[32 bits])
              - Node((Bool ? Bits | Bits)[32 bits])
                - Node(Bits(Int))
                  - Node((toplevel/vexiis_0_logic_core/_zz_AlignerPlugin_logic_extractors_0_ctx_instruction :  Bits[8 bits]))
                    - Node(Bits ## Bits)
                      - Node(Bits -> Bits)
                        - Node((toplevel/vexiis_0_logic_core/_zz_AlignerPlugin_logic_extractors_0_redo_7 :  Bool))
                          - Node(Bits(Int))
                            - Node((toplevel/vexiis_0_logic_core/AlignerPlugin_logic_extractors_0_slicesOh :  Bits[8 bits]))
                              - Node((toplevel/vexiis_0_logic_core/_zz_AlignerPlugin_logic_extractors_0_slicesOh :  Bits[8 bits]))
                                - Node(Bool && Bool)
                                  - Node((toplevel/vexiis_0_logic_core/AlignerPlugin_logic_extractors_0_usableMask_bools_0 :  Bool))
                                    - Node(Bits(Int))
                                      - Node((toplevel/vexiis_0_logic_core/_zz_AlignerPlugin_logic_extractors_0_usableMask_bools_0 :  Bits[8 bits]))
                                        - Node((toplevel/vexiis_0_logic_core/AlignerPlugin_logic_extractors_0_usableMask :  Bits[8 bits]))
                                          - Node(Bits ## Bits)
                                            - Node(Bits -> Bits)
                                              - Node(Bool && Bool)
                                                - Node((toplevel/vexiis_0_logic_core/AlignerPlugin_logic_scanners_7_valid :  Bool))
                                                  - Node(Bool && Bool)
                                                    - Node((toplevel/vexiis_0_logic_core/AlignerPlugin_logic_scanners_7_checker_0_valid :  Bool))
                                                      - Node((toplevel/vexiis_0_logic_core/AlignerPlugin_logic_scanners_7_checker_0_present :  Bool))
                                                        - Node(Bits(Int))
                                                          - Node((toplevel/vexiis_0_logic_core/AlignerPlugin_logic_slices_mask :  Bits[8 bits]))
                                                            - Node(Bits ## Bits)
                                                              - Node((Bool ? Bits | Bits)[4 bits])
                                                                - Node((toplevel/vexiis_0_logic_core/fetch_logic_ctrls_2_down_valid :  Bool))
                                                    - Node(Bool || Bool)
                                                      - Node(| Bits)
                                                        - Node(Bits ## Bits)
                                                          - Node(Bits -> Bits)
                                                            - Node((toplevel/vexiis_0_logic_core/AlignerPlugin_logic_scanners_7_checker_0_redo :  Bool))
                                                              - Node(Bool && Bool)
                                                                - Node(Bool && Bool)
                                                                  - Node(Bits(Int))
                                                                    - Node((toplevel/vexiis_0_logic_core/AlignerPlugin_logic_slices_last :  Bits[8 bits]))
                                                                      - Node(Bits ## Bits)
                                                                        - Node((Bool ? Bits | Bits)[4 bits])
                                                                          - Node((toplevel/vexiis_0_logic_core/fetch_logic_ctrls_2_down_valid :  Bool))
 */