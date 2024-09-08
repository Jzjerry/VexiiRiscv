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
  val W8, W4, W2, W1 = newElement()
}

object MiCoCompute extends AreaObject {
    // Symmetric Precision Dot Product
    def DotProductSym(op_a : Bits, op_b : Bits, bit_width : Int) : SInt = {
        // Let's assume XLEN == 32 here
        val vlen = 32 / bit_width
        val a_vec = Range(vlen, 0, -1).map{
            i => op_a(i*bit_width-1 downto (i-1)*bit_width).asSInt}
        val b_vec = Range(vlen, 0, -1).map{
            i => op_b(i*bit_width-1 downto (i-1)*bit_width).asSInt}
        val a_tmp = Vec(a_vec.zip(b_vec).map{ case (a_i, b_i) => a_i * b_i})
        a_tmp.reduceBalancedTree(_ +^ _).resize(32)
    }

    def DotProductSym2Bit(op_a : Bits, op_b : Bits) : SInt = {
        // TODO: Could Be Optimized
        val a_vec = Range(16, 0, -1).map{
            i => op_a(i*2-1 downto (i-1)*2).asSInt}
        val b_vec = Range(16, 0, -1).map{
            i => op_b(i*2-1 downto (i-1)*2).asSInt}
        val a_tmp = Vec(a_vec.zip(b_vec).map{ case (a_i, b_i) => a_i * b_i})
        a_tmp.reduceBalancedTree(_ +^ _).resize(32)
    }

    def DotProductSym1Bit(op_a : Bits, op_b : Bits) : SInt = {
        val xor = (op_a ^ op_b).asBools
        val count_n = xor.sCount(True)         // True is -1
        (S(32) - (count_n << 1).asSInt).resize(32)
    }

}

object MiCoUtil extends AreaObject {
    def CountIndexMSB(op : Bits) : UInt = {
        val vec_op = op.reversed.asBools
        val (isFound, idx) = vec_op.sFindFirst(_ === True)
        (isFound ? ~idx | U(0))
    }
}

object MiCoSymPlugin {
    // 4  * 8-bit vector DotP 4  * 8-bit vector
    val DOTP8   = IntRegFile.TypeR(M"0000000----------100-----0001011")
    // 8  * 4-bit vector DotP 8  * 4-bit vector
    val DOTP4   = IntRegFile.TypeR(M"0000000----------101-----0001011")
    // 16 * 2-bit vector DotP 16 * 2-bit vector
    val DOTP2   = IntRegFile.TypeR(M"0000000----------110-----0001011")
    // 32 * 1-bit vector DotP 32 * 1-bit vector
    val DOTP1   = IntRegFile.TypeR(M"0000000----------111-----0001011")

    val IDX_MSB = IntRegFile.TypeR(M"0000000----------000-----0001011")

    val WIDTH = Payload(DOTPType())
    val COMPUTE = Payload(Bool())
}

class MiCoSymPlugin(val layer : LaneLayer) extends ExecutionUnitElementSimple(layer)  {

    val logic = during setup new Logic {
        awaitBuild()

        //Let's assume we only support RV32 for now
        assert(Riscv.XLEN.get == 32)

        //Let's get the hardware interface that we will use to provide the result of our custom instruction
        val wb = newWriteback(ifp, 0)
        
        import MiCoSymPlugin._
        import SrcKeys._
        import DOTPType._

        add(DOTP8).srcs(SRC1.RF, SRC2.RF).decode(WIDTH -> W8, COMPUTE -> True)
        add(DOTP4).srcs(SRC1.RF, SRC2.RF).decode(WIDTH -> W4, COMPUTE -> True)
        add(DOTP2).srcs(SRC1.RF, SRC2.RF).decode(WIDTH -> W2, COMPUTE -> True)
        add(DOTP1).srcs(SRC1.RF, SRC2.RF).decode(WIDTH -> W1, COMPUTE -> True)

        add(IDX_MSB).srcs(SRC1.RF, SRC2.RF).decode(WIDTH -> W8, COMPUTE -> False)

        //Now that we are done specifying everything about the instructions, we can release the Logic.uopRetainer
        //This will allow a few other plugins to continue their elaboration (ex : decoder, dispatcher, ...)
        uopRetainer.release()

        //Let's define some logic in the execute lane [0]
        val process = new el.Execute(id = 0) {
          //Get the RISC-V RS1/RS2 values from the register file
          val rs1 = el(IntRegFile, RS1)
          val rs2 = el(IntRegFile, RS2)

          //Do some computation
          val rd = Bits(32 bits)

          val compute_res = WIDTH.muxList(
              List( (W8, MiCoCompute.DotProductSym(rs1, rs2, 8)),
                    (W4, MiCoCompute.DotProductSym(rs1, rs2, 4)),
                    (W2, MiCoCompute.DotProductSym(rs1, rs2, 2)),
                    (W1, MiCoCompute.DotProductSym1Bit(rs1, rs2)))).asBits

          val idx1 = MiCoUtil.CountIndexMSB(rs1)
          val idx2 = MiCoUtil.CountIndexMSB(rs2)

          rd := Mux(COMPUTE, 
                    compute_res, ((idx1 > idx2) ? idx1 | idx2).asBits.resize(32))
            
          //Provide the computation value for the writeback
          wb.valid := SEL
          wb.payload := rd
        }
    }
}
