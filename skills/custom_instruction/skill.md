# Create Custom Instruction Plugin

Add custom RISC-V instructions to VexiiRiscv using plugins. This enables the full agentic flow: design hardware → compile software → validate in simulation.

## Trigger Words
- custom instruction
- custom plugin
- simd add
- new opcode
- vexii plugin

## Overview

VexiiRiscv uses a plugin-based architecture. Custom instructions are implemented by creating Scala plugins that integrate with the execution pipeline using `ExecutionUnitElementSimple`.

## Full Agentic Flow

### Step 1: Create Hardware Plugin (Scala)

Create file `src/main/scala/vexiiriscv/execute/MyCustomPlugin.scala`:

```scala
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

// Define your instruction encoding
object MyCustomPlugin {
  // R-type format: opcode + rd + rs1 + rs2 + func3 + func7
  // Using Custom0 (0x0B) opcode
  val MY_OP = IntRegFile.TypeR(M"0000000----------000-----0001011")
}

class MyCustomPlugin(val layer: LaneLayer) extends ExecutionUnitElementSimple(layer) {

  val logic = during setup new Logic {
    awaitBuild()

    // Support RV32 only for this example
    assert(Riscv.XLEN.get == 32)

    // Create writeback interface
    val wb = newWriteback(ifp, 0)

    // Register custom instruction
    val myInst = add(MyCustomPlugin.MY_OP).spec

    // Specify register operands (read at stage 0)
    myInst.addRsSpec(RS1, executeAt = 0)
    myInst.addRsSpec(RS2, executeAt = 0)

    // Allow other plugins to continue
    uopRetainer.release()

    // Implement execution logic
    val process = new el.Execute(id = 0) {
      // Read source registers
      val rs1 = up(el(IntRegFile, RS1)).asUInt
      val rs2 = up(el(IntRegFile, RS2)).asUInt

      // Your custom computation
      val rd = rs1 + rs2  // Example: add operation

      // Writeback result
      wb.valid := SEL
      wb.payload := rd.asBits
    }
  }
}

// Generator App - generates Verilog
object VexiiMyCustomGen extends App {
  val param = new ParamSimple()
  val sc = SpinalConfig()

  assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("prints this usage text")
    param.addOptions(this)
  }.parse(args, ()).nonEmpty)

  val report = sc.generateVerilog {
    val pa = param.pluginsArea()
    pa.plugins += new MyCustomPlugin(pa.early0)
    ParamSimple.setPma(pa.plugins)
    VexiiRiscv(pa.plugins)
  }
}

// Simulator App - runs simulation
object VexiiMyCustomSim extends App {
  val param = new ParamSimple()
  val testOpt = new TestOptions()

  val genConfig = SpinalConfig()
  genConfig.includeSimulation

  val simConfig = SpinalSimConfig()
  simConfig.withFstWave
  simConfig.withTestFolder
  simConfig.withConfig(genConfig)

  assert(new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("prints this usage text")
    testOpt.addOptions(this)
    param.addOptions(this)
  }.parse(args, ()).nonEmpty)

  println(s"With Vexiiriscv parm :\n - ${param.getName()}")
  val compiled = simConfig.compile {
    val pa = param.pluginsArea()
    pa.plugins += new MyCustomPlugin(pa.early0)
    ParamSimple.setPma(pa.plugins)
    VexiiRiscv(pa.plugins)
  }
  testOpt.test(compiled)
}
```

### Step 2: Create Software Test (Assembly)

Create `mytest/src/test.S`:

```asm
.option arch, +zicsr

.global _start
_start:

#include "../../driver/riscv_asm.h"
#include "../../driver/sim_asm.h"
#include "../../driver/custom_asm.h"

    # Test custom instruction
    li x1, 10          # First operand
    li x2, 20          # Second operand
    opcode_R(CUSTOM0, 0x0, 0x00, x3, x1, x2)  # x3 = custom(x1, x2)

    # Print result
    li x4, PUT_HEX
    sw x3, 0(x4)

    # Check result
    li x5, 30           # Expected: 10 + 20 = 30
    bne x3, x5, fail

    j pass

pass:
    j pass
fail:
    j fail
```

### Step 3: Compile Software

```bash
cd mytest
make clean rv32im
# Output: build/mytest.elf
```

### Step 4: Run Simulation

```bash
cd ..
sbt "runMain vexiiriscv.execute.VexiiMyCustomSim --load-elf mytest/build/mytest.elf --trace-all --no-rvls-check"
```

Note: `--no-rvls-check` is required because Spike doesn't know about our custom instruction.

## Custom Instruction Encoding

RISC-V custom instructions use the Custom0 or Custom1 opcodes:

| Opcode | Value |
|--------|-------|
| CUSTOM0 | 0x0B |
| CUSTOM1 | 0x2B |

Use the `custom_asm.h` macros:
```asm
#define opcode_R(opcode, func3, func7, rd, rs1, rs2) \
    .word ((opcode) | (rd << 7) | (rs1 << 15) | (rs2 << 20) | (func3 << 12) | (func7 << 25))
```

## Key Plugin Components

| Component | Description |
|-----------|-------------|
| `ExecutionUnitElementSimple` | Base class for ALU-like plugins |
| `LaneLayer` | Execution lane (early0, lane0, etc.) |
| `add(instruction)` | Register custom instruction |
| `addRsSpec(RS1, ...)` | Specify register operands |
| `newWriteback()` | Create result writeback interface |
| `el.Execute` | Execution stage logic |

## Integration Points

Add plugin to different pipeline stages:
- `pa.early0` - Early execution stage (simple ALU)
- `pa.lane0` - Main execution lane
- Other lanes for more complex operations

## Example: SIMD Add Instruction

This implements 4x8-bit SIMD addition:

```
RD( 7 downto  0) = RS1( 7 downto  0) + RS2( 7 downto  0)
RD(16 downto  8) = RS1(16 downto  8) + RS2(16 downto  8)
RD(23 downto 16) = RS1(23 downto 16) + RS2(23 downto 16)
RD(31 downto 24) = RS1(31 downto 24) + RS2(31 downto 24)
```

See [SimdAddPlugin.scala](src/main/scala/vexiiriscv/execute/SimdAddPlugin.scala) for complete implementation.

## End-to-End Validation

1. Write hardware plugin → 2. Compile to Verilog → 3. Write software test → 4. Compile ELF → 5. Run simulation → 6. Verify results

All steps can be automated for continuous validation of custom instructions.
