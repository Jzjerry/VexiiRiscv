package vexiiriscv.decode

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline.{Connector, CtrlConnector}
import spinal.lib.misc.plugin.FiberPlugin
import vexiiriscv.execute.ExecuteUnitService
import vexiiriscv.fetch.FetchPipelinePlugin
import vexiiriscv.misc.PipelineService
import vexiiriscv.riscv
import Decode._
import spinal.lib.logic.{DecodingSpec, Masked, Symplify}
import vexiiriscv.riscv.{PC_READ, RD, RS1, RS2, RS3, Resource, RfAccess, RfResource, SingleDecoding}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class DecoderPlugin(decodeAt : Int = 2) extends FiberPlugin {
  lazy val dpp = host[DecodePipelinePlugin]
  addLockable(dpp)

  val logic = during build new Area{
    Decode.INSTRUCTION_WIDTH.set(32)

    val eus = host.list[ExecuteUnitService]
    val microOps = eus.flatMap(_.getMicroOp())
    val resources = microOps.flatMap(_.resources).distinctLinked
    val rfAccesses = mutable.LinkedHashSet[RfAccess]()
    resources.foreach{
      case r : RfResource => rfAccesses += r.access
      case _ =>
    }

    val rfaKeys = mutable.LinkedHashMap[RfAccess, AccessKeys]()
    for(rfa <- rfAccesses){
      val physWidth = 5
      val rfMapping = resources.collect{case r : RfResource if r.access == rfa => r.rf }.toList
      val ak = AccessKeys(physWidth, rfMapping)
      ak.setCompositeName(rfa)
      rfaKeys(rfa) = ak
    }
    Decode.rfaKeys.set(rfaKeys)

    val singleDecodings = mutable.LinkedHashSet[SingleDecoding]()
    microOps.foreach {
      case sd: SingleDecoding => singleDecodings += sd
    }


    val encodings = new Area {
      val all = mutable.LinkedHashSet[Masked]()
      val one = Masked(1, 1)
      val zero = Masked(0, 1)
      class RfAccessDecoding(val rfa : RfAccess){
        val rfaKey = rfaKeys(rfa)
        val read = new DecodingSpec(Bool()).setDefault(zero)
        val rfid = new DecodingSpec(UInt(rfaKey.rfIdWidth bits)).setDefault(Masked(0, (1 << rfaKey.rfIdWidth)-1))
      }
      val rfAccessDec = rfAccesses.map(rfa => rfa -> new RfAccessDecoding(rfa)).toMapLinked()
//      val readRs1, readRs2, readRs3, writeRd, fpSpec, rmSpec = new DecodingSpec(Bool()).setDefault(zero)
//      val regfileRs1, regfileRs2, regfileRs3, regfileRd = new DecodingSpec(REGFILE_RD())
//      val resourceToSpec = resourceToStageable.keys.map(_ -> new DecodingSpec(Bool()).setDefault(zero)).toMap
//      val regfileSelMask = (1 << setup.keys.regfileSelWidth) - 1
//      var withRs3 = false
      for (e <- singleDecodings) {
        val key = Masked(e.key)
        all += key

        e.resources.foreach {
          case r: RfResource => {
            val dec = rfAccessDec(r.access)
            dec.read.addNeeds(key, one)
            dec.rfid.addNeeds(key,  Masked(dec.rfaKey.idOf(r.rf), (1 << dec.rfaKey.rfIdWidth)-1))
          }
          case PC_READ =>
//          case INSTRUCTION_SIZE =>
//          case FPU => fpSpec.addNeeds(key, one)
//          case RM => rmSpec.addNeeds(key, one)
//          case r if resourceToStageable.contains(r) => resourceToSpec(r).addNeeds(key, one)
//          case naxriscv.interfaces.SQ =>
//          case naxriscv.interfaces.LQ =>
        }
      }
    }

    val decodeCtrl = dpp.ctrl(decodeAt)
    val laneLogic = for(laneId <- 0 until Decode.LANES) yield new decodeCtrl.Area(laneId) {
      LEGAL := Symplify(Decode.INSTRUCTION, encodings.all)
      for(rfa <- rfAccesses){
        val keys = rfaKeys(rfa)
        val dec = encodings.rfAccessDec(rfa)
        keys.READ := dec.read.build(Decode.INSTRUCTION, encodings.all)
        keys.RFID := dec.rfid.build(Decode.INSTRUCTION, encodings.all)
        keys.PHYS := Decode.INSTRUCTION(rfa match {
          case RS1 => riscv.Const.rs1Range
          case RS2 => riscv.Const.rs2Range
          case RS3 => riscv.Const.rs3Range
          case RD  => riscv.Const.rdRange
        }).asUInt
      }
    }

  }
}
