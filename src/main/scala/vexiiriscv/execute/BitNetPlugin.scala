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
  val BNSTORE = IntRegFile.TypeR(M"0000000----------010-----0001011")
}


class BitNetPlugin(
  val layer : LaneLayer, 
  val bufferWidth : Int = 0
  ) extends ExecutionUnitElementSimple(layer)  {

  val logic = during setup new Logic {
    awaitBuild()

    //Let's assume we only support RV32 for now
    assert(Riscv.XLEN.get == 32)

    //Let's get the hardware interface that we will use to provide the result of our custom instruction
    val wb = newWriteback(ifp, 0)
    
    //Specify that the current plugin will implement the ADD4 instruction
    val bnadd4 = add(BitNetPlugin.BNSUM).spec

    //We need to specify on which stage we start using the register file values
    bnadd4.addRsSpec(RS1, executeAt = 0)
    bnadd4.addRsSpec(RS2, executeAt = 0)

    if(bufferWidth > 0){
      val bnstore = add(BitNetPlugin.BNSTORE).spec
      bnstore.addRsSpec(RS1, executeAt = 0)
      if (bufferWidth > 32){
        bnstore.addRsSpec(RS2, executeAt = 0)
        }
    }

    //Now that we are done specifying everything about the instructions, we can release the Logic.uopRetainer
    //This will allow a few other plugins to continue their elaboration (ex : decoder, dispatcher, ...)
    uopRetainer.release()

    //Let's define some logic in the execute lane [0]
    val process = new el.Execute(id = 0) {
      //Get the RISC-V RS1/RS2 values from the register file
      val rs1 = el(IntRegFile, RS1).asUInt
      val rs2 = el(IntRegFile, RS2).asBits

      //Do some computation
      val rd = SInt(32 bits)

      val a_vec = Range(4, 0, -1).map{i => rs1(i*8-1 downto i*8-8).asSInt}
      // val w_vec = Range(4, 0, -1).map{i => rs2(i*2-1 downto i*2-2)} 
      val w_vec = Range(1, 5, 1).map{i => rs2(i*2-1 downto i*2-2)} 

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
      rd := a_tmp.reduce(_ +^ _).resized

      //Provide the computation value for the writeback
      wb.valid := SEL
      wb.payload := rd.asBits
    }
  }
}


