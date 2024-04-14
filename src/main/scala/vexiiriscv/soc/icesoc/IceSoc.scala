package vexiiriscv.soc.icesoc

import rvls.spinal.RvlsBackend
import spinal.core._
import spinal.core.sim._
import spinal.core.fiber._
import spinal.lib.{ResetCtrlFiber, StreamPipe}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.fabric.Node
import spinal.lib.com.uart.TilelinkUartFiber
import spinal.lib.com.uart.sim.{UartDecoder, UartEncoder}
import spinal.lib.cpu.riscv.RiscvHart
import spinal.lib.cpu.riscv.debug.DebugModuleFiber
import spinal.lib.eda.bench.Rtl
import spinal.lib.misc.{Elf, PathTracer, TilelinkClintFiber}
import spinal.lib.misc.plic.TilelinkPlicFiber
import spinal.lib.system.tag.PMA
import vexiiriscv.ParamSimple
import vexiiriscv.execute.SrcPlugin
import vexiiriscv.misc.TrapPlugin
import vexiiriscv.soc.TilelinkVexiiRiscvFiber
import vexiiriscv.test.VexiiRiscvProbe

import vexiiriscv._

import java.io.File
import scala.collection.mutable.ArrayBuffer

class IceSoc extends Component {
  val asyncReset = in Bool()
  val cd100 = ClockDomain.external("cd100", withReset = false, frequency = FixedFrequency(100 MHz))

  val debugResetCtrl = cd100(new ResetCtrlFiber().addAsyncReset(asyncReset, HIGH))
  val mainResetCtrl  = cd100(new ResetCtrlFiber().addAsyncReset(debugResetCtrl))


  val main = mainResetCtrl.cd on new Area {
    val sharedBus = tilelink.fabric.Node()

    val param = new ParamSimple()
    param.fetchForkAt = 1
    param.withMul = true
    param.withDiv = true
    param.fetchL1Enable = true
    param.fetchL1Ways = 1
    param.lsuPmaAt = 1
    param.lsuForkAt = 1
    param.relaxedBranch = true
    
    val plugins = param.plugins()
    val cpu = new TilelinkVexiiRiscvFiber(plugins)
    sharedBus << cpu.buses
    cpu.dBus.setDownConnection(a = StreamPipe.S2M)

    val ram = new tilelink.fabric.RamFiber(48 KiB)
    ram.up at 0x80000000l of sharedBus

    // Handle all the IO / Peripheral things
    val peripheral = new Area {
      val busXlen = Node().forceDataWidth(param.xlen)
      busXlen << sharedBus
      busXlen.setUpConnection(a = StreamPipe.HALF, d = StreamPipe.HALF)

      val bus32 = Node().forceDataWidth(32)
      bus32 << busXlen

      val clint = new TilelinkClintFiber()
      clint.node at 0x10010000 of busXlen

      val plic = new TilelinkPlicFiber()
      plic.node at 0x10C00000 of bus32

      val uart = new TilelinkUartFiber()
      uart.node at 0x10001000 of bus32
      plic.mapUpInterrupt(1, uart.interrupt)

      val cpuPlic = cpu.bind(plic)
      val cpuClint = cpu.bind(clint)
    }

    val patches = Fiber build new Area{
      ram.thread.logic.mem.generateAsBlackBox()
    }
  }
}

object IceSocGen extends App{
  val config = SpinalConfig(targetDirectory = "hw/rtl").addStandardMemBlackboxing(blackboxOnlyIfRequested)
  val report = SpinalVerilog(config){
    new IceSoc()
  }

  val h = report.toplevel.main.cpu.logic.core.host
  val path = PathTracer.impl(h[SrcPlugin].logic.addsub.rs2Patched, h[TrapPlugin].logic.harts(0).trap.pending.state.tval)
  println(path.report)
}

object RateIceSocCore extends App{
  val core = VexiiRiscv((new IceSoc()).main.plugins)
}