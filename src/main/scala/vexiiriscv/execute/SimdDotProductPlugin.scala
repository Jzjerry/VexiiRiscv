package vexiiriscv.execute

import spinal.core._
import spinal.core.sim.SpinalSimConfig
import spinal.lib._
import spinal.lib.pipeline.Stageable
import vexiiriscv.decode
import spinal.lib.misc.pipeline._
import vexiiriscv.Generate.args
import vexiiriscv.{Global, ParamSimple, VexiiRiscv}
import vexiiriscv.compat.MultiPortWritesSymplifier
import vexiiriscv.riscv.{IntRegFile, RS1, RS2, Riscv}
import vexiiriscv.tester.TestOptions

object DOTPType extends SpinalEnum(defaultEncoding=binaryOneHot){
  val W8, W4, W2 = newElement()
}

object SimdDotProductPlugin{
  //Define the instruction type and encoding that we wll use
  val DOTP8 = IntRegFile.TypeR(M"0000000----------100-----0001011") // 4  * 8-bit vector DotP 4  * 8-bit vector
  val DOTP4 = IntRegFile.TypeR(M"0000000----------101-----0001011") // 8  * 4-bit vector DotP 8  * 4-bit vector
  val DOTP2 = IntRegFile.TypeR(M"0000000----------110-----0001011") // 16 * 2-bit vector DotP 16 * 2-bit vector

  val WIDTH = Payload(DOTPType())

  def DotProductRes(op_a : Bits, op_b : Bits, bit_width : Int) : SInt = {

    // Let's assume XLEN == 32 here
    val vlen = 32 / bit_width
    val a_vec = Range(vlen, 0, -1).map{i => op_a(i*bit_width-1 downto (i-1)*bit_width).asSInt}
    val b_vec = Range(vlen, 0, -1).map{i => op_b(i*bit_width-1 downto (i-1)*bit_width).asSInt}
    val a_tmp = Vec(a_vec.zip(b_vec).map{ case (a_i, b_i) => a_i * b_i})
    a_tmp.reduceBalancedTree(_ +^ _).resize(32)
  }
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
    import SimdDotProductPlugin._
    import SrcKeys._
    import DOTPType._

    add(DOTP8).srcs(SRC1.RF, SRC2.RF).decode(WIDTH -> W8)
    add(DOTP4).srcs(SRC1.RF, SRC2.RF).decode(WIDTH -> W4)
    add(DOTP2).srcs(SRC1.RF, SRC2.RF).decode(WIDTH -> W2)

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

      rd := WIDTH.muxList(
          List(
            (W8, SimdDotProductPlugin.DotProductRes(rs1, rs2, 8)),
            (W4, SimdDotProductPlugin.DotProductRes(rs1, rs2, 4)),
            (W2, SimdDotProductPlugin.DotProductRes(rs1, rs2, 2))
            )
        )

      //Provide the computation value for the writeback
      wb.valid := SEL
      wb.payload := rd.asBits
    }
  }
}