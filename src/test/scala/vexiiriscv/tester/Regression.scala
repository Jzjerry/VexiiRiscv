package vexiiriscv.tester

import org.apache.commons.io.FileUtils
import spinal.core._
import spinal.core.sim._
import spinal.lib.misc.plugin.Hostable
import spinal.lib.misc.test.{AsyncJob, MultithreadedFunSuite}
import vexiiriscv.memory.MmuPlugin
import vexiiriscv.misc.PrivilegedPlugin
import vexiiriscv.riscv.Riscv
import vexiiriscv.{Global, ParamSimple, VexiiRiscv}

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext
import scala.reflect.io.Path.jfile2path
import scala.util.Random

class RegressionSingle(compiled : SimCompiled[VexiiRiscv],
                       dutArgs : Seq[String] = Nil,
                       freertosCount : Int = sys.env.getOrElse("VEXIIRISCV_REGRESSION_FREERTOS_COUNT", "1").toInt,
                       withBuildroot: Boolean = sys.env.getOrElse("VEXIIRISCV_REGRESSION_BUILDROOT_ENABLED", "1").toInt.toBoolean) {
  val dut = compiled.dut
  val xlen = dut.database(Riscv.XLEN)
  val priv = dut.host.get[PrivilegedPlugin]
  val mmu = dut.host.get[MmuPlugin]

  val rvm = dut.database(Riscv.RVM)
  val rvc = dut.database(Riscv.RVC)
  val rvf = dut.database(Riscv.RVF)
  val rvd = dut.database(Riscv.RVD)
  val rva = dut.database(Riscv.RVA)

  var arch = ""
  var archLinux = ""

  if (xlen == 64) {
    arch = "rv64i"
    archLinux = "rv64i"
  } else {
    arch = "rv32i"
    archLinux = "rv32i"
  }
  if (rvm) {
    arch += "m"
    archLinux += "m"
  }
  if (rva) {
    arch += "a"
    archLinux += "a"
  }
  if (rvf) {
    arch += "f"
    archLinux += "f"
  }
  if (rvd) {
    arch += "d"
    archLinux += "d"
  }
  if(rvc) {
    arch += "c"
    archLinux += "c"
  }


  if(List("im", "imc").exists(arch.endsWith)){
    arch = arch.replace("im", "ima")
  }


  def newTest() = {
    val t = new TestOptions()
    tests += t
    t
  }

  def newArgs() = {
    val t = new TestArgs()
    testArgs += t
    t.noStdin()
    t.ibusReadyFactor(0.5)
    t.dbusReadyFactor(0.5)
    t
  }
  //
//
//  def newTest(args : Seq[String]) = {
//    val t = newTest()
//    assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
//      help("help").text("prints this usage text")
//      t.addOptions(this)
//    }.parse(args, Unit).nonEmpty)
//    t
//  }

  val tests = ArrayBuffer[TestOptions]()
  val testArgs = ArrayBuffer[TestArgs]()

  val nsf = new File("ext/NaxSoftware")

  val rejectedTests = mutable.LinkedHashSet("rv32ui-p-simple", "rv32ui-p-fence_i", "rv64ui-p-simple", "rv64ui-p-fence_i", "rv32ua-p-lrsc", "rv64ua-p-lrsc")

  //rvi tests
  val riscvTestsFile = new File(nsf, "riscv-tests")
  val riscvTests = riscvTestsFile.listFiles().sorted
  val rvti = riscvTests.filter{ t => val n = t.getName; n.startsWith(s"rv${xlen}ui-p-") && !n.contains(".") && !rejectedTests.contains(n) }
  val rvtm = riscvTests.filter { t => val n = t.getName; n.startsWith(s"rv${xlen}um-p-") && !n.contains(".") && !rejectedTests.contains(n)  }
  val rvta = riscvTests.filter { t => val n = t.getName; n.startsWith(s"rv${xlen}ua-p-") && !n.contains(".") && !rejectedTests.contains(n)  }

  val riscvTestsFrom2 = ArrayBuffer[File]()
  riscvTestsFrom2 ++= rvti
  if(rvm) riscvTestsFrom2 ++= rvtm
  if(dut.database(Riscv.RVA)) riscvTestsFrom2 ++= rvta

  for(elf <- riscvTestsFrom2) {
    val args = newArgs()
    args.loadElf(elf)
    args.failAfter(100000)
    args.startSymbol("test_2")
    args.name("riscv-tests/" + elf.getName)
  }

  if(rva){
    val args = newArgs()
    args.loadElf(new File(nsf, s"riscv-tests/rv${xlen}ua-p-lrsc"))
    args.failAfter(1000000)
    args.startSymbol("test_2")
    args.passSymbol("test_5")
    args.name(s"riscv-tests/rv${xlen}ua-p-lrsc")
  }

  def doArchTest(from : String) = {
    val folder = s"riscv-arch-test/rv${xlen}i_m/$from"
    val elfs = new File(nsf, folder).listFiles().filter(_.getName.endsWith(".elf"))
    for (elf <- elfs) {
      val args = newArgs()
      args.loadElf(elf)
      args.failAfter(10000000)
      args.name(folder + "/" + elf.getName.replace(".elf", ""))
    }
  }

  doArchTest("I")
  doArchTest("Zifencei")
  doArchTest("privilege")
  if (rvm) doArchTest("M")
  if (rvc) doArchTest("C")


  val regulars = ArrayBuffer("dhrystone", "coremark_vexii", "machine_vexii")
  priv.filter(_.p.withSupervisor).foreach(_ => regulars ++= List("supervisor"))
  if(mmu.nonEmpty) regulars ++= List(s"mmu_sv${if(xlen == 32) 32 else 39}")
  for(name <- regulars){
    val args = newArgs()
    args.loadElf(new File(nsf, s"baremetal/$name/build/$arch/$name.elf"))
    args.failAfter(300000000)
    args.name(s"regular/$name")
  }

  val benchmarks = ArrayBuffer("dhrystone", "coremark_vexii")
  for (name <- benchmarks) {
    val args = newArgs()
    args.loadElf(new File(nsf, s"baremetal/$name/build/$arch/$name.elf"))
    args.failAfter(300000000)
    args.ibusReadyFactor(2.0)
    args.dbusReadyFactor(2.0)
    args.name(s"benchmark/$name")
  }


  val freertos = List(
    "sp_flop", "blocktim", "integer", "countsem", "EventGroupsDemo", "flop", "QPeek",
    "QueueSet", "recmutex", "semtest", "TaskNotify", "dynamic",
    "GenQTest", "PollQ", "QueueOverwrite", "QueueSetPolling", "test1"
  )
  if(rvm) for(name <- freertos.take(freertosCount)){
    val args = newArgs()
    args.loadElf(new File(nsf,  f"baremetal/freertosDemo/build/${name}/${arch + (arch.endsWith("im").mux("a",""))}/freertosDemo.elf"))
    args.failAfter(300000000)
    args.name(s"freertos/$name")
  }

  if(withBuildroot && rvm && rva && mmu.nonEmpty) priv.filter(_.p.withSupervisor).foreach{ _ =>
    val path = s"ext/NaxSoftware/buildroot/images/$archLinux"
    val args = newArgs()
    args.failAfter(10000000000l)
    args.name("buildroot")
    args.loadBin(0x80000000l, s"$path/fw_jump.bin")
    args.loadBin(0x80F80000l, s"$path/linux.dtb")
    args.loadBin(0x80400000l, s"$path/Image")
    args.loadBin(0x81000000l, s"$path/rootfs.cpio")

    args.fsmGetc("buildroot login:")
    args.fsmSleep(100000*10)
    args.fsmPutc("root"); args.fsmPutcLr()
    args.fsmGetc("#")
    args.fsmPutc("cat /proc/cpuinfo"); args.fsmPutcLr()
    args.fsmGetc("#")
    args.fsmPutc("echo 1+2+3*4 | bc"); args.fsmPutcLr()
    args.fsmGetc("#")
    args.fsmPutc("micropython"); args.fsmPutcLr()
    args.fsmGetc(">>> ")
    args.fsmPutc("import math"); args.fsmPutcLr()
    args.fsmGetc(">>> ")
    args.fsmPutc("math.sin(math.pi/4)"); args.fsmPutcLr()
    args.fsmGetc(">>> ")
    args.fsmPutc("from sys import exit"); args.fsmPutcLr()
    args.fsmGetc(">>> ")
    args.fsmPutc("exit()"); args.fsmPutcLr()
    args.fsmGetc("#")
    args.fsmPutc("ls /"); args.fsmPutcLr()
    args.fsmGetc("#")
    args.fsmSuccess()
  }

  implicit val ec = ExecutionContext.global
  val jobs = ArrayBuffer[AsyncJob]()

  val tp = new File(compiled.simConfig.getTestPath(""))
  FileUtils.forceMkdir(tp)
  val argsFile = new BufferedWriter(new FileWriter(new File(tp, "args")))
  argsFile.write(dutArgs.map(v => if (v.contains(" ")) '"' + v + '"' else v).mkString(" "))
  argsFile.close()

  for(args <- testArgs){
    val t = newTest()
    assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
      help("help").text("prints this usage text")
      t.addOptions(this)
    }.parse(args.args, Unit).nonEmpty)
    
    val testPath = new File(compiled.simConfig.getTestPath(t.testName.get))
    val passFile = new File(testPath, "PASS")
    val failFile = new File(testPath, "FAIL")
    FileUtils.deleteQuietly(passFile)
    FileUtils.deleteQuietly(failFile)

    val testName = t.testName.get
    if(!passFile.exists()){
      val stdoutHost = Console.out
      val job = new AsyncJob(toStdout = false, logsPath = testPath)({
        FileUtils.forceMkdir(testPath)
        val argsFile = new BufferedWriter(new FileWriter(new File(testPath, "args")))
        argsFile.write(args.args.map(v => if(v.contains(" ")) '"' + v + '"' else v).mkString(" "))
        argsFile.close()

        t.test(compiled)
        val bf = new BufferedWriter(new FileWriter(passFile))
        bf.flush()
        bf.close()
        stdoutHost.println(s"PASS $testName")
      }){
        override def onFail() = {
          val bf = new BufferedWriter(new FileWriter(failFile))
          bf.flush()
          bf.close()
          stdoutHost.println(s"FAIL $testName")
        }
      }
      jobs += job
    }
  }
  jobs.foreach(_.join())
}

object RegressionSingle extends App{
  def test(name : String, plugins : => Seq[Hostable], dutArgs : Seq[String]): Unit = {
    val simConfig = SpinalSimConfig()
    simConfig.withFstWave
    simConfig.setTestPath("regression/$COMPILED_tests/$TEST")
    val compiled = SpinalConfig.synchronized(simConfig.compile(VexiiRiscv(plugins).setDefinitionName(s"VexiiRiscv_$name")))
    val regression = new RegressionSingle(compiled, dutArgs)
    println("*" * 80)
    val fails = regression.jobs.filter(_.failed)
    if (fails.isEmpty) {
      println("PASS")
      return
    }
    println(s"FAILED ${fails.size}/${regression.jobs.size}")
    for (fail <- fails) {
      println("- " + fail.logsFile.getAbsolutePath)
    }
    Thread.sleep(10)
    throw new Exception()
  }

  def test(ps : ParamSimple, dutArgs : Seq[String] = Nil): Unit = {
    test(ps.getName(), TestBench.paramToPlugins(ps), dutArgs)
  }

  def test(args : String) : Unit = test(args.split(" "))
  def test(args : Seq[String]): Unit = {
    val param = new ParamSimple()
    assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
      help("help").text("prints this usage text")
      param.addOptions(this)
    }.parse(args, Unit).nonEmpty)
    test(param, args)
  }

  test(args)
}


class Regression extends MultithreadedFunSuite(sys.env.getOrElse("VEXIIRISCV_REGRESSION_THREAD_COUNT", "0").toInt){
  FileUtils.deleteQuietly(new File("regression"))

  val testsAdded = mutable.LinkedHashSet[String]()
  def addTest(args: String): Unit = addTest(args.replace("  ", " ").split("\\s+"))
  def addTest(args: Seq[String]): Unit = {
    val param = new ParamSimple()
    assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
      help("help").text("prints this usage text")
      param.addOptions(this)
    }.parse(args, Unit).nonEmpty)

    val paramName = param.getName()
    if(testsAdded.contains(paramName)) return
    testsAdded += paramName
    testMp(paramName) {
      RegressionSingle.test(param, args)
    }
  }


  abstract class Dimensions(val name : String){
    def getPositions() : Seq[String]
  }

  val dimensions = ArrayBuffer[Dimensions]()
  def addDim(name : String, poses : Seq[String]) = dimensions += new Dimensions(name){
    override def getPositions(): Seq[String] = poses
  }

  addDim("lanes", List(1, 2).map(v => s"--lanes $v"))
  addDim("rf", List("--regfile-sync", "--regfile-async"))
  addDim("bypass", List(0,0,0,1,2,3,100).map(v => s"--allow-bypass-from $v")) //More weight to fully bypassed configs
  addDim("xlen", List(32, 64).map(v => s"--xlen $v"))
  addDim("prediction", List("--with-btb", "--with-btb --with-ras", "--with-btb --with-ras --with-gshare"))
  addDim("priv", List("", "--with-supervisor", "--with-user"))
  addDim("rvm", List("--without-mul --without-div", "--with-mul --with-div"))
  addDim("divParam", List(2, 4).flatMap(radix => List("", "--div-ipc").map(opt => s"$opt --div-radix $radix")))
  addDim("rva", List("", "--with-mul --with-div --with-rva"))
  addDim("rvc", List("", "--with-mul --with-div --with-rvc"))
  addDim("late-alu", List("", "--with-late-alu"))
  addDim("fetch", {
    val p = ArrayBuffer[String]("--fetch-fork-at 0", "--fetch-fork-at 1")
    for (bytes <- List(1 << 10, 1 << 12, 1 << 14);
         sets <- List(16, 32, 64)) {
      if (bytes / sets >= 64) {
        val ways = bytes / sets / 64
        p += s"--with-fetch-l1 --fetch-l1-sets=$sets --fetch-l1-ways=$ways"
      }
    }
    p
  })
  addDim("lsu", {
    val p = ArrayBuffer[String]("--lsu-fork-offset 0", "--lsu-fork-offset 1")
    for(bytes <- List(1 << 10, 1 << 12, 1 << 14);
      sets <- List(16 , 32, 64)){
      if(bytes / sets >= 64) {
        val ways = bytes / sets / 64
        p += s"--with-lsu-l1 --lsu-l1-sets=$sets --lsu-l1-ways=$ways"
      }
    }
    p
  })
  addDim("lsu bypass", List("", "--with-lsu-bypass"))
  addDim("ishift", List("", "--with-iterative-shift"))

  val default = "--with-mul --with-div --performance-counters 4"

  // Add a simple test for each dimensions's positions
  for(dim <- dimensions){
    for(pos <- dim.getPositions()) {
      addTest(default + " " + pos)
    }
  }

  // Generate random parameters
  val random = new Random(42)
  for(i <- 0 until 50){
    val args = ArrayBuffer[String]()
    args += default
    for (dim <- dimensions) {
      args += dim.getPositions().randomPick(random)
    }
    addTest(args.mkString(" "))
  }
}

//cd $PWD && find . -name FAIL && find . -name PASS | wc -l && find . -name FAIL | wc -l