package vexiiriscv.execute

import spinal.core._
import spinal.lib.misc.pipeline._
import vexiiriscv.decode
import vexiiriscv.riscv._

import vexiiriscv.execute._

object BitNetBufferPlugin extends AreaObject{
  //Define the instruction type and encoding that we wll use
  val BNSUM   = IntRegFile.TypeR(M"0000000----------001-----0001011")
  val BNSTORE = IntRegFile.TypeR(M"0000000----------010-----0001011")

  val STORE = Payload(Bool())
  val RESULT = Payload(SInt(32 bits))
}

class BitNetBufferPlugin(val layer : LaneLayer,
    val QType : String = "1.5b",
    val bufferWidth : Int = 8) extends ExecutionUnitElementSimple(layer)  {


  val w_width = QType match {
    case "1b" => 1
    case "1.5b" => 2
    case "2b" => 2
  }

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
      val buffer = Reg(Bits(w_width*bufferWidth bits)) init(0)
      //Get the RISC-V RS1/RS2 values from the register file
      val rs1 = el(IntRegFile, RS1).asUInt
      val rs2 = el(IntRegFile, RS2).asUInt

      import BitNetPlugin.bitnetadd4

      //Do some computation
      val rd = SInt(32 bits)

      val data = (rs1 ## rs2).asBits

      // val a_vec = Range(8, 0, -1).map{i => data(i*8-1 downto i*8-8).asSInt}
      // val w_vec = Range(8, 0, -1).map{i => buffer(i*w_width-1 downto (i-1)*w_width)}

      // We assume the little-endian format
      val buffer_high = buffer(8*w_width - 1 downto 4*w_width).asUInt
      val buffer_low = buffer(4*w_width - 1 downto 0).asUInt

      if(QType == "1b"){
        RESULT := (bitnetadd4(rs1, buffer_high, QType) + 
                bitnetadd4(rs2, buffer_low, QType)).resized
      } else {
        RESULT := (bitnetadd4(rs1, buffer_low, QType) + 
                  bitnetadd4(rs2, buffer_high, QType)).resized
      }

      when (SEL & STORE){
        buffer := data(w_width*bufferWidth-1 downto 0)
      }
    }
    val store = new el.Execute(id = 0) {
      wb.valid := SEL
      wb.payload := RESULT.asBits
    }
  }
}


