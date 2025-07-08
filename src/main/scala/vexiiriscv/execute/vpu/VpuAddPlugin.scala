package vexiiriscv.execute.vpu

import spinal.core._
import spinal.core.sim.SpinalSimConfig
import spinal.lib._
import spinal.lib.misc.plugin.FiberPlugin
import spinal.lib.misc.pipeline._

import vexiiriscv.execute._
import vexiiriscv.Generate.args
import vexiiriscv.{Global, ParamSimple, VexiiRiscv}
import vexiiriscv.compat.MultiPortWritesSymplifier
import vexiiriscv.riscv.{VectorRegFile, RS1, RS2, Riscv}
import vexiiriscv.tester.TestOptions

class VpuAddPlugin(val layer: LaneLayer) extends FiberPlugin {

  val logic = during setup new Area {
    val wbp = host.find[WriteBackPlugin](
      p => p.rf == VectorRegFile && p.lane == layer.lane)
    val earlyLock = retains(layer.lane.uopLock, wbp.elaborationLock)
    val lateLock = retains(layer.lane.pipelineLock)
    awaitBuild()

    val VADD = VectorRegFile.TypeR(M"0000000----------000-----0001011")

    val vadd = layer.add(VADD)
    val vlen = Riscv.VLEN

    vadd.addRsSpec(RS1, executeAt = 0)
    vadd.addRsSpec(RS2, executeAt = 0)
    vadd.setCompletion(0)

    val wb = wbp.createPort(at = 0)
    wbp.addMicroOp(wb, vadd)

    val SEL = Payload(Bool())
    layer.lane.setDecodingDefault(SEL, False)
    vadd.addDecoding(SEL -> True)

    earlyLock.release()

    //Let's define some logic in the execute lane [0]
    val process = new layer.Execute(id = 0) {
      //Get the RISC-V RS1/RS2 values from the register file
      val rs1 = layer.lane(VectorRegFile, RS1)
      val rs2 = layer.lane(VectorRegFile, RS2)

      //Do some computation
      val rd = Bits(vlen bits)
      def VecAdd(op_a : Bits, op_b : Bits, elemWidth : Int) : Bits = {
        val a_vec = op_a.subdivideIn(elemWidth bits)
        val b_vec = op_b.subdivideIn(elemWidth bits)
        // Element-wise addition
        val result = a_vec.zip(b_vec).map { 
          case (a, b) => (a.asSInt + b.asSInt).asBits }
        // Concatenate the results back into a single vector
        result.reduce(_ ## _).resize(vlen bits)
      }
      rd := VecAdd(rs1, rs2, 8) // Assuming 8-bit elements for this example
      //Provide the computation value for the writeback
      wb.valid := isValid && SEL
      wb.payload := rd
    }
    lateLock.release()
  }
}