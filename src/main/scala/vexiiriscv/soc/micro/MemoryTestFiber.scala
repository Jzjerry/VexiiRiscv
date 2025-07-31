package vexiiriscv.soc.micro

import spinal.core._
import spinal.core.fiber._

import spinal.lib._

import spinal.lib.bus._
import spinal.lib.bus.misc._
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.fabric._
import scala.collection.mutable.ArrayBuffer

object MemoryTestFiber {
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
}

class MemoryTest(dBusParam: BusParameter) extends Component {
    val io = new Bundle {
        val dBus = master(tilelink.Bus(dBusParam))
        val data = out Bits(32 bits)
    }

    val addrWidth = dBusParam.addressWidth
    val memValid = RegInit(False)
    val memReady = RegInit(False)
    
    val accessAddr = Reg(UInt(addrWidth bits)) init(0x80000000l)

    val dBus = io.dBus // short alias
    dBus.a.opcode  := tilelink.Opcode.A.GET
    dBus.a.param   := 0
    dBus.a.source  := 0
    dBus.a.data    := 0
    dBus.a.address := accessAddr
    dBus.a.mask    := B"1111"
    dBus.a.size    := 3 // 32 bits
    dBus.a.corrupt := False
    dBus.a.valid := memValid
    dBus.d.ready := memReady

    val dataBuffer = Reg(Bits(32 bits)) init(0)
    io.data := dataBuffer
            
    val timeout = Timeout(100000000) // Ten Cycles Timeout
    when(timeout) {

        when(!memReady){
            memValid := True
        }

        when (dBus.a.fire){
            memValid := False
            memReady := True
        }

        when (dBus.d.fire) {
            memReady := False
            dataBuffer := dBus.d.data
            accessAddr := accessAddr + 4 // Increment address for next read
            timeout.clear()
            report(L"[Memory Test] Read data 0x${dBus.d.data} from address 0x$accessAddr")
        }
    }
}

class MemoryTestFiber extends Area{
    val dBus = Node.down()

    import MemoryTestFiber._

    val logic = Fiber build new Area{
        dBus.m2s forceParameters getM2sParameters(MemoryTestFiber.this)
        dBus.s2m.supported load tilelink.S2mSupport.none()

        val test = new MemoryTest(dBus.bus.p)
        test.io.dBus <> dBus.bus

        val test_out = test.io.data.toIo()
    }
}