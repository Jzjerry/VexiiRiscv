package vexiiriscv.execute

import spinal.core._
import spinal.core.sim.SpinalSimConfig
import spinal.lib._
import spinal.lib.pipeline.Stageable
import vexiiriscv.Generate.args
import vexiiriscv.{Global, ParamSimple, VexiiRiscv}
import vexiiriscv.compat.MultiPortWritesSymplifier
import vexiiriscv.riscv.{IntRegFile, RS1, RS2, Riscv}
import vexiiriscv.tester.TestOptions

object SimdDotProductPlugin{
  //Define the instruction type and encoding that we wll use
  val DOTP = IntRegFile.TypeR(M"1100000----------000-----0001011")
}

class SimdDotProductPlugin(val layer : LaneLayer) extends ExecutionUnitElementSimple(layer)  {

  //Here we create an elaboration thread. The Logic class is provided by ExecutionUnitElementSimple to provide functionalities
  val logic = during setup new Logic {
    //Here we could have lock the elaboration of some other plugins (ex CSR), but here we don't need any of that
    //as all is already sorted out in the Logic base class.
    //So we just wait for the build phase
    awaitBuild()

    //Let's assume we only support RV32 for now
    assert(Riscv.XLEN.get == 32)

    //Let's get the hardware interface that we will use to provide the result of our custom instruction
    val wb = newWriteback(ifp, 0)
    
    //Specify that the current plugin will implement the ADD4 instruction
    val dotp = add(SimdDotProductPlugin.DOTP).spec

    //We need to specify on which stage we start using the register file values
    dotp.addRsSpec(RS1, executeAt = 0)
    dotp.addRsSpec(RS2, executeAt = 0)

    //Now that we are done specifying everything about the instructions, we can release the Logic.uopRetainer
    //This will allow a few other plugins to continue their elaboration (ex : decoder, dispatcher, ...)
    uopRetainer.release()

    //Let's define some logic in the execute lane [0]
    val process = new el.Execute(id = 0) {
      //Get the RISC-V RS1/RS2 values from the register file
      val rs1 = el(IntRegFile, RS1)
      val rs2 = el(IntRegFile, RS2)

      //Do some computation
      val rd = SInt(32 bits)

      val a_vec = Range(4, 0, -1).map{i => rs1(i*8-1 downto i*8-8).asSInt}
      val b_vec = Range(4, 0, -1).map{i => rs2(i*8-1 downto i*8-8).asSInt}

      val a_tmp = Vec(a_vec.zip(b_vec).map{ case (a_i, b_i) => a_i * b_i})
      val res = a_tmp.reduceBalancedTree(_ +^ _)

      rd := res.resized

      //Provide the computation value for the writeback
      wb.valid := SEL
      wb.payload := rd.asBits
    }
  }
}