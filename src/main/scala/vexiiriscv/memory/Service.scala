package vexiiriscv.memory

import spinal.core._
import spinal.core.fiber.Retainer
import spinal.lib._
import spinal.lib.misc.pipeline._
import spinal.lib.misc.plugin._
import vexiiriscv.Global
import vexiiriscv.Global._
import vexiiriscv.riscv.Riscv

import scala.collection.mutable.ArrayBuffer

trait AddressTranslationPortUsage
object AddressTranslationPortUsage{
  object FETCH extends AddressTranslationPortUsage
  object LOAD_STORE extends AddressTranslationPortUsage
}

trait AddressTranslationService extends Area {
  val elaborationLock = Retainer()
  def newStorage(pAny: Any): Any

  def newTranslationPort(nodes: Seq[NodeBaseApi],
                         rawAddress: Payload[UInt],
                         allowRefill: Payload[Bool],
                         usage: AddressTranslationPortUsage,
                         portSpec: Any,
                         storageSpec: Any): AddressTranslationRsp
}

class AddressTranslationRsp(s : AddressTranslationService, wakesCount : Int, val wayCount : Int) extends Area{
  val keys = new Area {
    setName("MMU")
    val TRANSLATED = Payload(PHYSICAL_ADDRESS)
    val IO = Payload(Bool())
    val REDO = Payload(Bool())
    val ALLOW_READ, ALLOW_WRITE, ALLOW_EXECUTE = Payload(Bool())
    val PAGE_FAULT = Payload(Bool())
    val ACCESS_FAULT = Payload(Bool())
    val WAYS_OH  = Payload(Bits(wayCount bits))
    val WAYS_PHYSICAL  = Payload(Vec.fill(wayCount)(PHYSICAL_ADDRESS()))
    val BYPASS_TRANSLATION = Payload(Bool())
  }
  val wake = Bool()
//  val pipelineLock = Retainer().retain()
}


trait DBusAccessService{
  def newDBusAccess() : DBusAccess = dbusAccesses.addRet(new DBusAccess)
  val dbusAccesses = ArrayBuffer[DBusAccess]()
  val accessRetainer = Retainer()
}

case class DBusAccess() extends Bundle {
  val cmd = Stream(DBusAccessCmd())
  val rsp = Flow(DBusAccessRsp())
}

case class DBusAccessCmd() extends Bundle {
  val address = Global.PHYSICAL_ADDRESS()
  val size = UInt(2 bits)
}

case class DBusAccessRsp() extends Bundle {
  val data = Bits(Riscv.XLEN bits)
  val error = Bool()
  val redo = Bool()
}
