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


object BitNetPlugin{
  //Define the instruction type and encoding that we wll use
  val BNSUM   = IntRegFile.TypeR(M"0000000----------001-----0001011")
  def bitnetadd4(a: UInt, w : UInt, QType : String = "1.5b") : SInt = {
    val w_width = QType match {
      case "1b" => 1
      case "1.5b" => 2
      case "2b" => 2
    }
    // We assume the little-endian format
    val a_vec = Range(4, 0, -1).map{i => a(i*8-1 downto i*8-8).asSInt}
    // val w_vec = Range(4, 0, -1).map{i => w(i*w_width-1 downto (i-1)*w_width)} 
    val w_vec = Range(1, 5, 1).map{i => w(i*w_width-1 downto (i-1)*w_width)} 
    val a_tmp = a_vec.zip(w_vec).map{
        case (a_i, w_i) => {
            val a_tmp_i = SInt(8 bits)
            if (w_width == 2){
              when(w_i === B"01".asUInt){
                  a_tmp_i := a_i
              }.elsewhen(w_i === B"11".asUInt){
                  a_tmp_i := -a_i
              }.elsewhen(w_i === B"10".asUInt){
                if(QType == "1.5b"){
                  a_tmp_i := 0
                }
                else{
                  a_tmp_i := -(a_i |<< 1)
                }
              }.otherwise{
                  a_tmp_i := 0
              }
            } else{
              when(w_i === B"0".asUInt){
                a_tmp_i := a_i
              }.otherwise{
                a_tmp_i := -a_i
              }
            }
            a_tmp_i
        }
    }
    a_tmp.reduce(_ +^ _)
  }
}


class BitNetPlugin(
  val layer : LaneLayer, 
  val QType : String = "1.5b") extends ExecutionUnitElementSimple(layer)  {


  val w_width = QType match {
    case "1b" => 1
    case "1.5b" => 2
    case "2b" => 2
  }

  val logic = during setup new Logic {
    awaitBuild()

    //Let's assume we only support RV32 for now
    assert(Riscv.XLEN.get == 32)

    //Let's get the hardware interface that we will use to provide the result of our custom instruction
    val wb = newWriteback(ifp, 0)
    
    //Specify that the current plugin will implement the ADD4 instruction
    val bnsum = add(BitNetPlugin.BNSUM).spec

    //We need to specify on which stage we start using the register file values
    bnsum.addRsSpec(RS1, executeAt = 0)
    bnsum.addRsSpec(RS2, executeAt = 0)

    //Now that we are done specifying everything about the instructions, we can release the Logic.uopRetainer
    //This will allow a few other plugins to continue their elaboration (ex : decoder, dispatcher, ...)
    uopRetainer.release()

    //Let's define some logic in the execute lane [0]
    val process = new el.Execute(id = 0) {
      //Get the RISC-V RS1/RS2 values from the register file
      val rs1 = el(IntRegFile, RS1).asUInt
      val rs2 = el(IntRegFile, RS2).asUInt

      //Do some computation
      val rd = SInt(32 bits)

      rd := BitNetPlugin.bitnetadd4(rs1, rs2, QType).resized

      //Provide the computation value for the writeback
      wb.valid := SEL
      wb.payload := rd.asBits
    }
  }
}


