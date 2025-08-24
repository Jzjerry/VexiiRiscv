package vexiiriscv.soc

import spinal.core
import spinal.core._
import spinal.core.fiber._

import spinal.lib._
import spinal.lib.bus._
import spinal.lib.bus.misc._
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.fabric._
import spinal.lib.misc._

import scala.collection.mutable.ArrayBuffer

import vexiiriscv.execute.cfu._

/* A Template for a Tilelink CFU (Custom Functional Unit) Fiber.
 * This fiber connects a CFU bus to CPU, allowing custom instructions
 * to be executed in a VexiiRiscv system.
 */

object TilelinkCfuFiber {
  def getTilelinkSupport(proposed: bus.tilelink.M2sSupport) = bus.tilelink.SlaveFactory.getSupported(
        addressWidth = 32,
        dataWidth = 32,
        allowBurst = false,
        proposed = proposed
    )

  def getM2sParameters(name: Nameable) = tilelink.M2sParameters(
        addressWidth = 32,
        dataWidth = 32,
        masters = List(
          tilelink.M2sAgent(
            name = name,
            mapping = List(
              tilelink.M2sSource(
                id = SizeMapping(0, 1),
                emits = M2sTransfers(
                  get = tilelink.SizeRange(1, 32 / 8),
                  putFull = tilelink.SizeRange(1, 32 / 8)
                )
              )
            )
          )
        )
      )
  
  def getCfuBusParameters = CfuBusParameter(
        CFU_VERSION = 0,
        CFU_INTERFACE_ID_W = 0,
        CFU_FUNCTION_ID_W = 3,
        CFU_REORDER_ID_W = 0,
        CFU_REQ_RESP_ID_W = 0,
        CFU_INPUTS = 2,
        CFU_INPUT_DATA_W = 32,
        CFU_OUTPUTS = 1,
        CFU_OUTPUT_DATA_W = 32,
        CFU_FLOW_REQ_READY_ALWAYS = false,
        CFU_FLOW_RESP_READY_ALWAYS = false,
        CFU_WITH_STATUS = true,
        CFU_RAW_INSN_W = 32,
        CFU_CFU_ID_W = 4,
        CFU_STATE_INDEX_NUM = 5
      )

    def elemwiseAdd(opa: Bits, opb: Bits, elemWidth: Int) : Bits = {
        require(opa.getBitsWidth == opb.getBitsWidth, "Operands must have the same bit width")
        require(opa.getBitsWidth % elemWidth == 0, "Element width must divide the operand width")
        val veca = opa.subdivideIn(elemWidth bits)
        val vecb = opb.subdivideIn(elemWidth bits)
        val vecRes = veca.zip(vecb).map { case (a, b) => a.asSInt + b.asSInt }
        vecRes.asBits
    }

    def reduceDotProd(opa: Bits, opb: Bits, elemWidth: Int): Bits = {
        require(opa.getBitsWidth == opb.getBitsWidth, "Operands must have the same bit width")
        require(opa.getBitsWidth % elemWidth == 0, "Element width must divide the operand width")
        val veca = opa.subdivideIn(elemWidth bits)
        val vecb = opb.subdivideIn(elemWidth bits)
        val dotProd = veca.zip(vecb).map { case (a, b) => a.asSInt * b.asSInt }.reduce(_ + _)
        dotProd.asBits.resized
    }
}

class TilelinkCfuFiber() extends Area {

  import TilelinkCfuFiber._

  val bus = Node.down()
  val cfuParam = getCfuBusParameters
  val dBus = bus.bus

  val logic = Fiber build new Area{
      bus.m2s forceParameters getM2sParameters(TilelinkCfuFiber.this)
      bus.s2m.supported load tilelink.S2mSupport.none()

      val cfuBus = CfuBus(cfuParam)

      /* Handle CFU Default Logic */
      cfuBus.rsp.arbitrationFrom(cfuBus.cmd)
      cfuBus.rsp.response_id := cfuBus.cmd.request_id

      if (cfuParam.CFU_WITH_STATUS) cfuBus.rsp.status := B"000" 

      val cfuRes = cfuBus.rsp.outputs(0)
      val func3 = cfuBus.cmd.function_id.asBits

      cfuRes := func3.mux(
        B"000" -> elemwiseAdd(cfuBus.cmd.inputs(0), cfuBus.cmd.inputs(1), 8),
        B"001" -> reduceDotProd(cfuBus.cmd.inputs(0), cfuBus.cmd.inputs(1), 8),
        default -> B(0)
      )

      /* Handle Tilelink Default Logic */
      dBus.a.opcode  := tilelink.Opcode.A.GET
      dBus.a.param   := 0
      dBus.a.source  := 0
      dBus.a.data    := 0
      dBus.a.address := 0
      dBus.a.mask    := B"1111"
      dBus.a.size    := 3 // 32 bits
      dBus.a.corrupt := False
      dBus.a.valid := False
      dBus.d.ready := False
  }
}