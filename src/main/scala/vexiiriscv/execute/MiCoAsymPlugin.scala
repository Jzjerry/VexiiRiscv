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

object AsymDOTPType extends SpinalEnum(defaultEncoding=binaryOneHot){
  val W8, W4, W2, W1 = newElement()
}

/*
    For now we only support 8-bit x 1-,2-,4-bit
*/

object MiCoAsymCompute extends AreaObject {
    def DotProductAsym(op_a : Bits, op_b : Bits) : SInt = {
        // Let's assume XLEN == 32 here
        val vlen = 4
        val a_vec = op_a.subdivideIn(vlen slices)
        val b_vec = op_b.subdivideIn(vlen slices)
        val a_tmp = Vec(a_vec.zip(b_vec).map{case (a_i, b_i) => a_i.asSInt * b_i.asSInt})
        a_tmp.reduceBalancedTree(_ +^ _).resize(32)
    }
    def DotProductAsym2b(op_a : Bits, op_b : Bits) : SInt = {
        // Let's assume XLEN == 32 here
        val vlen = 4 // 4 x 8-bit integer vector
        val a_vec = op_a.subdivideIn(vlen slices)
        val b_vec = op_b.subdivideIn(vlen slices)

        val a_tmp = a_vec.zip(b_vec).map{
            case (a_i, w_i) => {
                val a_tmp_i = SInt(8 bits)
                val neg_a  = -(a_i.asSInt)
                val neg_2a = neg_a |<< 1
                a_tmp_i := w_i.muxList(
                    List((B"2'b01", a_i.asSInt),
                        ( B"2'b11", neg_a),
                        ( B"2'b10", neg_2a),
                        ( B"2'b00", S(0))))
                a_tmp_i
            }
        }
        // A more accurate implementation, but larger area
        // val a_tmp = Vec(a_vec.zip(b_vec).map{case (a_i, b_i) => a_i.asSInt * b_i.asSInt})
        a_tmp.reduceBalancedTree(_ +^ _).resize(32)
    }
    def DotProductAsym1b(op_a : Bits, op_b : Bits) : SInt = {
        // Let's assume XLEN == 32 here
        val vlen = 4 // 4 x 8-bit integer vector
        val a_vec = op_a.subdivideIn(vlen slices)
        val b_vec = op_b.subdivideIn(vlen slices)
        val a_tmp = Vec(a_vec.zip(b_vec).map{
            case (a_i, b_i) => (b_i.asBool ? -a_i.asSInt | a_i.asSInt)})
        a_tmp.reduceBalancedTree(_ +^ _).resize(32)
    }
    def Extend1bTo2b(op : Bits) : Bits = {
        // A simple map of:
        // 0 -> 01 (+1 in 2-bit)
        // 1 -> 11 (-1 in 2-bit)
        // So that we can re-use some logic
        val ext = op.subdivideIn(1 bits).reverse.map{
            i => i ## B"1"}.reduce(_ ## _)
        ext
    }
    def Extend2bTo4b(op : Bits) : Bits = {
        // Sign Extend 2-bit to 4-bit
        val ext = op.subdivideIn(2 bits).reverse.map{
            i => i.asSInt.resize(4).asBits
        }.reduce(_ ## _)
        ext
    }
    def ExtendTo8b(op : Bits) : Bits = {
        // Sign Extend 4-bit to 8-bit
        val ext = op.subdivideIn(4 slices).reverse.map{
            i => i.asSInt.resize(8).asBits
        }.reduce(_ ## _)
        ext
    }
    // def Extend1p5To2b(op : Bits) : Bits = {
    //     // Decoder that decode 1.58-bit to 2-bit
    // }
}


object MiCoAsymPlugin {
    // 8 * 8-bit vector DotP 8  * 8-bit vector
    val DOTP8x8   = IntRegFile.TypeR(M"0000001----------100-----0001011")
    // 8  * 4-bit vector DotP 8  * 4-bit vector
    val DOTP4x8   = IntRegFile.TypeR(M"0000001----------101-----0001011")
    // 16 * 2-bit vector DotP 16 * 2-bit vector
    val DOTP2x8   = IntRegFile.TypeR(M"0000001----------110-----0001011")
    // 32 * 1-bit vector DotP 32 * 1-bit vector
    val DOTP1x8   = IntRegFile.TypeR(M"0000001----------111-----0001011")

    val WIDTH = Payload(AsymDOTPType())
}

class MiCoAsymPlugin(val layer : LaneLayer, val MaxWidth : Int = 1) 
    extends ExecutionUnitElementSimple(layer)  {

    val logic = during setup new Logic {
        awaitBuild()

        //Let's assume we only support RV32 for now
        assert(Riscv.XLEN.get == 32)

        //Let's get the hardware interface that we will use to provide the result of our custom instruction
        val wb = newWriteback(ifp, 0)
        
        import MiCoAsymPlugin._
        import MiCoAsymCompute._
        import SrcKeys._
        import AsymDOTPType._


        if(MaxWidth == 8) add(DOTP8x8).srcs(SRC1.RF, SRC2.RF).decode(WIDTH -> W8)
        if(MaxWidth >= 4) add(DOTP4x8).srcs(SRC1.RF, SRC2.RF).decode(WIDTH -> W4)
        if(MaxWidth >= 2) add(DOTP2x8).srcs(SRC1.RF, SRC2.RF).decode(WIDTH -> W2)
        add(DOTP1x8).srcs(SRC1.RF, SRC2.RF).decode(WIDTH -> W1)

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
            val offset = Reg(UInt(log2Up(32) bits)) init(0)
            
            val length = WIDTH.muxList(
                List((W8, U(0)),(W4, U(16)),(W2, U(8)),(W1, U(4)))
            )

            when(isValid && SEL && WIDTH =/= W8){
                offset := offset + length
            }
            
            val ToDotP8x8 = rs2.asBits
            val ToDotP4x8 = rs2(offset, 16 bits)
            val ToDotP2x8 = rs2(offset,  8 bits)
            val ToDotP1x8 = rs2(offset,  4 bits)
            
            val Ext1to8 = ExtendTo8b(Extend1bTo2b(ToDotP1x8)).asBits
            val Ext2to8 = ExtendTo8b(ToDotP2x8).asBits
            val Ext4to8 = ExtendTo8b(Extend2bTo4b(ToDotP4x8)).asBits

            val ToDotP = Bits(MaxWidth*4 bits)
            if (MaxWidth == 8){
                // Hardware Unpacker to Extend everything into 8-bit
                val Ext1to8 = ExtendTo8b(Extend1bTo2b(ToDotP1x8)).asBits
                val Ext2to8 = ExtendTo8b(ToDotP2x8).asBits
                val Ext4to8 = ExtendTo8b(Extend2bTo4b(ToDotP4x8)).asBits

                ToDotP := WIDTH.muxList(
                    List((W8, ToDotP8x8),(W4, Ext4to8),(W2, Ext2to8),(W1, Ext1to8))
                )
            }
            else if (MaxWidth == 4){
                val Ext1to4 = Extend2bTo4b(Extend1bTo2b(ToDotP1x8)).asBits
                val Ext2to4 = Extend2bTo4b(ToDotP2x8).asBits
                
                ToDotP := WIDTH.muxList(
                    List((W8, B(0)),(W4, ToDotP4x8),(W2, Ext2to4),(W1, Ext1to4))
                )
            }
            else if (MaxWidth == 2){
                val Ext1to2 = Extend1bTo2b(ToDotP1x8).asBits
                ToDotP := WIDTH.muxList(
                    List((W8, B(0)),(W4, B(0)),(W2, ToDotP2x8),(W1, Ext1to2))
                )
            }
            else if (MaxWidth == 1){
                ToDotP := WIDTH.muxList(
                    List((W8, B(0)),(W4, B(0)),(W2, B(0)),(W1, ToDotP1x8))
                )
            }

            val compute_res = if (MaxWidth == 8) DotProductAsym(rs1, ToDotP)
                              else if (MaxWidth == 4) DotProductAsym(rs1, ToDotP)
                              else if (MaxWidth == 2) DotProductAsym2b(rs1, ToDotP)
                              else DotProductAsym1b(rs1, ToDotP)

            rd := compute_res.asBits

            //Provide the computation value for the writeback
            wb.valid := SEL
            wb.payload := rd
        }
    }
}