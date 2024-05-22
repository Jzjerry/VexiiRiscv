package vexiiriscv.execute

import spinal.core._
import spinal.lib.misc.pipeline._
import vexiiriscv.decode
import vexiiriscv.riscv._


object BitNetBufferPlugin extends AreaObject{
  //Define the instruction type and encoding that we wll use
  val BNSUM   = IntRegFile.TypeR(M"0000000----------001-----0001011")
  val BNSTORE = IntRegFile.TypeR(M"0000000----------010-----0001011")

  val STORE = Payload(Bool())
  val RESULT = Payload(SInt(32 bits))
}

class BitNetBufferPlugin(val layer : LaneLayer, 
    val bufferWidth : Int = 8) extends ExecutionUnitElementSimple(layer)  {


  val logic = during setup new Logic {
    awaitBuild()
    import SrcKeys._

    //Let's assume we only support RV32 for now
    assert(Riscv.XLEN.get == 32)

    //Let's get the hardware interface that we will use to provide the result of our custom instruction
    val wb = newWriteback(ifp, 0)
    import BitNetBufferPlugin._
    add(BNSUM).srcs(SRC1.RF, SRC2.RF).decode(STORE -> False)
    add(BNSTORE).srcs(SRC1.RF, SRC2.RF).decode(STORE -> True)

    //Now that we are done specifying everything about the instructions, we can release the Logic.uopRetainer
    //This will allow a few other plugins to continue their elaboration (ex : decoder, dispatcher, ...)
    uopRetainer.release()

    //Let's define some logic in the execute lane [0]
    val compute = new el.Execute(id = 0) {
      val buffer = Reg(Bits(2*bufferWidth bits)) init(0)
      //Get the RISC-V RS1/RS2 values from the register file
      val rs1 = el(IntRegFile, RS1).asUInt
      val rs2 = el(IntRegFile, RS2).asUInt

      val data = rs1 ## rs2

      //Do some computation
      val rd = SInt(32 bits)

      val a_vec = Range(8, 0, -1).map{i => data(i*8-1 downto i*8-8).asSInt}
      // val w_vec = Range(1, 9, 1).map{i => buffer(i*2-1 downto i*2-2)}
      val w_vec = Range(8, 0, -1).map{i => buffer(i*2-1 downto i*2-2)}

      val a_tmp = a_vec.zip(w_vec).map{
          case (a_i, w_i) => {
              val a_tmp_i = SInt(8 bits)
                  when(w_i === B"01"){
                  a_tmp_i := a_i
              }.elsewhen(w_i === B"11"){
                  a_tmp_i := -a_i
              }.otherwise{
                  a_tmp_i := 0
              }
              a_tmp_i
          }
      }
      RESULT := a_tmp.reduce(_ +^ _).resized

      when (SEL & STORE){
        buffer := data(2*bufferWidth-1 downto 0).asBits
      }
    }
    val store = new el.Execute(id = 0) {
      wb.valid := SEL
      wb.payload := RESULT.asBits
    }
  }
}


