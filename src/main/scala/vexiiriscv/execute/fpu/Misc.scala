package vexiiriscv.execute.fpu

import spinal.core._
import spinal.lib.misc.pipeline.Payload
import vexiiriscv.riscv.Riscv
import vexiiriscv.riscv.Riscv.XLEN

object FpuUtils extends AreaObject {
  def rsFloatWidth = 32 + Riscv.RVD.get.toInt*32
  def rsIntWidth = Riscv.XLEN.get
  def exponentWidth = if(Riscv.RVD) 11 else 8
  def mantissaWidth = if(Riscv.RVD) 52 else 23
  def rvd = Riscv.RVD.get
  def rvf = Riscv.RVF.get
  def rvfhmin = Riscv.RVZfhmin.get
  def rvfh = Riscv.RVZfh.get
  def rv64 = XLEN.get == 64
  val exponentF16One = 15
  val exponentF32One = 127
  val exponentF64One = 1023
  val FORMAT = Payload(FpuFormat())
  val ROUNDING = Payload(FpuRoundMode())

  def whenHalf(format : FpuFormat.C)(yes : => Unit)(no : => Unit): Unit ={
    if(rvfhmin) when(format === FpuFormat.HALF) { yes } otherwise { no }
    if(!rvfhmin) no
  }

  def whenDouble(format : FpuFormat.C)(yes : => Unit)(no : => Unit): Unit ={
    if(rvd) when(format === FpuFormat.DOUBLE) { yes } otherwise{ no }
    if(!rvd) no
  }

  def muxDouble[T <: Data](format : FpuFormat.C)(yes : => T)(no : => T): T ={
    if(rvd) ((format === FpuFormat.DOUBLE) ? { yes } | { no })
    else no
  }
  def muxDouble[T <: Data](format : Bool)(yes : => T)(no : => T): T ={
    if(rvd) ((format) ? { yes } | { no })
    else no
  }
  def muxRv64[T <: Data](format : Bool)(yes : => T)(no : => T): T ={
    if(rv64) ((format) ? { yes } | { no })
    else no
  }

  def muxFormat[T <: Data](format : FpuFormat.C)(half : => T, float : => T, double : => T): T = {
    if(rvd && rvfhmin) (format === FpuFormat.DOUBLE) ? double | ((format === FpuFormat.HALF) ? half | float)
    else if(rvd) (format === FpuFormat.DOUBLE) ? double | float
    else if(rvfhmin) (format === FpuFormat.HALF) ? half | float
    else float
  }

  def formatMantissaWidth(format : FpuFormat.C): UInt = muxFormat[UInt](format)(U(10), U(23), U(52))
  def formatExponentWidth(format : FpuFormat.C): UInt = muxFormat[UInt](format)(U(5), U(8), U(11))
  def formatBias(format : FpuFormat.C): SInt = muxFormat[SInt](format)(S(exponentF16One), S(exponentF32One), S(exponentF64One))
  def formatExponentMax(format : FpuFormat.C): SInt = muxFormat[SInt](format)(S(exponentF16One), S(exponentF32One), S(exponentF64One))
  def formatExponentSubnormal(format : FpuFormat.C): SInt = -formatBias(format) + 1
  def formatStorageBits(format : FpuFormat.C): UInt = muxFormat[UInt](format)(U(16), U(32), U(64))
  def isHalf(format : FpuFormat.C): Bool = if(rvfhmin) format === FpuFormat.HALF else False
  def isFloat(format : FpuFormat.C): Bool = format === FpuFormat.FLOAT
  def isDouble(format : FpuFormat.C): Bool = if(rvd) format === FpuFormat.DOUBLE else False

  def unpackedConfig = FloatUnpackedParam(
    exponentMax = (1 << exponentWidth - 1) - 1,
    exponentMin = -(1 << exponentWidth - 1) + 1 - Riscv.fpuMantissaWidth,
    mantissaWidth = Riscv.fpuMantissaWidth
  )
}
