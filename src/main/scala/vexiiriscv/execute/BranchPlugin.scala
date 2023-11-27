// SPDX-FileCopyrightText: 2023 "Everybody"
//
// SPDX-License-Identifier: MIT

package vexiiriscv.execute

import spinal.core._
import spinal.lib.misc.pipeline._
import vexiiriscv.riscv.{IMM, RD, Riscv, Rvi}
import vexiiriscv._
import decode.Decode._
import Global._
import vexiiriscv.decode.Decode
import vexiiriscv.fetch.{Fetch, PcPlugin}
import vexiiriscv.schedule.ReschedulePlugin

object BranchPlugin extends AreaObject {
  val BranchCtrlEnum = new SpinalEnum(binarySequential) {
    val B, JAL, JALR = newElement()
  }
  val BRANCH_CTRL =  Payload(BranchCtrlEnum())
}

class BranchPlugin(val laneName : String,
                   var aluAt : Int = 0,
                   var jumpAt: Int = 1,
                   var wbAt: Int = 0) extends ExecutionUnitElementSimple(laneName)  {
  import BranchPlugin._
  lazy val wbp = host.find[WriteBackPlugin](_.laneName == laneName)
  lazy val sp = host[ReschedulePlugin]
  lazy val pcp = host[PcPlugin]
  setupRetain(wbp.elaborationLock)
  setupRetain(sp.elaborationLock)
  setupRetain(pcp.elaborationLock)

  val logic = during build new Logic{
    import SrcKeys._

    val wb = wbp.createPort(wbAt)
    wbp.addMicroOp(wb, Rvi.JAL, Rvi.JALR)

    add(Rvi.JAL ).decode(BRANCH_CTRL -> BranchCtrlEnum.JAL )
    add(Rvi.JALR).decode(BRANCH_CTRL -> BranchCtrlEnum.JALR).srcs(SRC1.RF)
    add(Rvi.BEQ ).decode(BRANCH_CTRL -> BranchCtrlEnum.B   ).srcs(SRC1.RF, SRC2.RF)
    add(Rvi.BNE ).decode(BRANCH_CTRL -> BranchCtrlEnum.B   ).srcs(SRC1.RF, SRC2.RF)
    add(Rvi.BLT ).decode(BRANCH_CTRL -> BranchCtrlEnum.B   ).srcs(SRC1.RF, SRC2.RF, Op.LESS  )
    add(Rvi.BGE ).decode(BRANCH_CTRL -> BranchCtrlEnum.B   ).srcs(SRC1.RF, SRC2.RF, Op.LESS  )
    add(Rvi.BLTU).decode(BRANCH_CTRL -> BranchCtrlEnum.B   ).srcs(SRC1.RF, SRC2.RF, Op.LESS_U)
    add(Rvi.BGEU).decode(BRANCH_CTRL -> BranchCtrlEnum.B   ).srcs(SRC1.RF, SRC2.RF, Op.LESS_U)

    eu.setCompletion(Math.max(jumpAt, wbAt), Rvi.JAL, Rvi.JALR)
    eu.setCompletion(jumpAt, Rvi.BEQ, Rvi.BNE, Rvi.BLT, Rvi.BGE, Rvi.BLTU, Rvi.BGEU)

    val age = eu.getExecuteAge(jumpAt)
    val pcPort = pcp.createJumpInterface(age, laneAgeWidth = Execute.LANE_AGE_WIDTH, aggregationPriority = 0)
//    val trapPort = if XXX sp.newTrapPort(age)
    val flushPort = sp.newFlushPort(eu.getExecuteAge(jumpAt), laneAgeWidth = Execute.LANE_AGE_WIDTH, withUopId = true)

    eu.uopLock.release()
    wbp.elaborationLock.release()
    sp.elaborationLock.release()
    srcp.elaborationLock.release()
    pcp.elaborationLock.release()

    val aluCtrl = eu.execute(aluAt)
    val alu = new aluCtrl.Area {
      val ss = SrcStageables
      val EQ = insert(ss.SRC1 === ss.SRC2)

      val COND = insert(BRANCH_CTRL.mux(
        BranchCtrlEnum.JALR -> True,
        BranchCtrlEnum.JAL -> True,
        BranchCtrlEnum.B -> UOP(14 downto 12).mux[Bool](
          B"000" ->  EQ,
          B"001" -> !EQ,
          M"1-1" -> !ss.LESS,
          default -> ss.LESS
        )
      ))

      val imm = IMM(UOP)
      val target_a = BRANCH_CTRL.mux(
        default -> S(PC),
        BranchCtrlEnum.JALR -> ss.SRC1.resize(PC_WIDTH)
      )

      val target_b = BRANCH_CTRL.mux(
        default -> imm.b_sext,
        BranchCtrlEnum.JAL -> imm.j_sext,
        BranchCtrlEnum.JALR -> imm.i_sext
      )

      val slices = Decode.INSTRUCTION_SLICE_COUNT +^ 1
      val sliceShift = Fetch.SLICE_RANGE_LOW.get
      val PC_TRUE = insert(U(target_a + target_b).as(PC)) //TODO overflows ?
      val PC_FALSE = insert(PC + (slices << sliceShift))
    }

    val jumpCtrl = eu.execute(jumpAt)
    val jumpLogic = new jumpCtrl.Area {
      val doIt = isValid && SEL && alu.COND

      pcPort.valid := doIt
      pcPort.pc := alu.PC_TRUE
      pcPort.laneAge := Execute.LANE_AGE

      flushPort.valid := doIt
      flushPort.hartId := Global.HART_ID
      flushPort.uopId :=  Decode.UOP_ID + 1
      flushPort.laneAge := Execute.LANE_AGE
      flushPort.self := False
    }

    val wbCtrl = eu.execute(wbAt)
    val wbLogic = new wbCtrl.Area{
      wb.valid := SEL && Decode.rfaKeys.get(RD).ENABLE
      wb.payload := alu.PC_FALSE.asBits
    }
  }
}
