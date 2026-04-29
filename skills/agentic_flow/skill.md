# Full Agentic Flow: Custom RISC-V Instruction

This skill enables fully agentic flow for creating custom RISC-V instructions in VexiiRiscv. It covers the complete workflow: design hardware → write software → validate in simulation.

## Trigger Words
- agentic flow
- full custom instruction
- end-to-end validation
- create custom opcode
- test custom instruction

## Complete Workflow

### Phase 1: Design Hardware (Scala Plugin)

Create your custom instruction plugin in `src/main/scala/vexiiriscv/execute/`:

```scala
// Example: SIMD Add (4x 8-bit additions)
package vexiiriscv.execute

import spinal.core._
import spinal.core.sim.SpinalSimConfig
import spinal.lib._
import vexiiriscv.Generate.args
import vexiiriscv.{ParamSimple, VexiiRiscv}
import vexiiriscv.riscv.{IntRegFile, RS1, RS2, Riscv}
import vexiiriscv.tester.TestOptions

object MySimdAddPlugin {
  // Custom0 opcode: 0000000----------000-----0001011
  val SIMD_ADD = IntRegFile.TypeR(M"0000000----------000-----0001011")
}

class MySimdAddPlugin(val layer: LaneLayer) extends ExecutionUnitElementSimple(layer) {
  val logic = during setup new Logic {
    awaitBuild()
    assert(Riscv.XLEN.get == 32)  // RV32 only

    val wb = newWriteback(ifp, 0)
    val inst = add(MySimdAddPlugin.SIMD_ADD).spec
    inst.addRsSpec(RS1, executeAt = 0)
    inst.addRsSpec(RS2, executeAt = 0)
    uopRetainer.release()

    val process = new el.Execute(id = 0) {
      val rs1 = up(el(IntRegFile, RS1)).asUInt
      val rs2 = up(el(IntRegFile, RS2)).asUInt

      // 4x 8-bit SIMD addition
      val rd = UInt(32 bits)
      rd( 7 downto  0) := rs1( 7 downto  0) + rs2( 7 downto  0)
      rd(16 downto  8) := rs1(16 downto  8) + rs2(16 downto  8)
      rd(23 downto 16) := rs1(23 downto 16) + rs2(23 downto 16)
      rd(31 downto 24) := rs1(31 downto 24) + rs2(31 downto 24)

      wb.valid := SEL
      wb.payload := rd.asBits
    }
  }
}

// Generator
object VexiiSimdAddGen extends App {
  val param = new ParamSimple()
  val sc = SpinalConfig()
  new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("")
    param.addOptions(this)
  }.parse(args, ()).nonEmpty

  sc.generateVerilog {
    val pa = param.pluginsArea()
    pa.plugins += new MySimdAddPlugin(pa.early0)
    ParamSimple.setPma(pa.plugins)
    VexiiRiscv(pa.plugins)
  }
}

// Simulator
object VexiiSimdAddSim extends App {
  val param = new ParamSimple()
  val testOpt = new TestOptions()
  val genConfig = SpinalConfig()
  genConfig.includeSimulation

  val simConfig = SpinalSimConfig()
  simConfig.withFstWave.withTestFolder.withConfig(genConfig)

  new scopt.OptionParser[Unit]("VexiiRiscv") {
    help("help").text("")
    testOpt.addOptions(this)
    param.addOptions(this)
  }.parse(args, ()).nonEmpty

  val compiled = simConfig.compile {
    val pa = param.pluginsArea()
    pa.plugins += new MySimdAddPlugin(pa.early0)
    ParamSimple.setPma(pa.plugins)
    VexiiRiscv(pa.plugins)
  }
  testOpt.test(compiled)
}
```

### Phase 2: Write Software Test

Create `mytest/src/test.S`:

```asm
.option arch, +zicsr

.global _start
_start:
#include "../../driver/sim_asm.h"
#include "../../driver/custom_asm.h"

    # Test SIMD_ADD: 0x01234567 + 0x01FF01FF
    li x1, 0x01234567
    li x2, 0x01FF01FF
    opcode_R(CUSTOM0, 0x0, 0x00, x3, x1, x2)

    # Print result
    li x4, PUT_HEX
    sw x3, 0(x4)

    # Expected: 0x02224666
    li x5, 0x02224666
    bne x3, x5, fail

    j pass

pass:
    j pass
fail:
    j fail
```

### Phase 3: Compile Software

```bash
cd mytest
make clean rv32im
# Output: build/mytest.elf
```

### Phase 4: Run Simulation

```bash
cd ..
sbt "runMain vexiiriscv.execute.VexiiSimdAddSim --load-elf mytest/build/mytest.elf --trace-all --no-rvls-check"
```

### Phase 5: Analyze Results

Check outputs in `simWorkspace/VexiiRiscv/test/`:
- `wave.fst` - View waveforms in gtkwave
- `konata.log` - View pipeline in Konata
- Console output shows test results

## Automation Script

Create `scripts/test_custom.sh`:

```bash
#!/bin/bash
# Test custom instruction end-to-end

PLUGIN_NAME="MyCustomPlugin"
TEST_NAME="mytest"
ELF_PATH="${TEST_NAME}/build/${TEST_NAME}.elf"

echo "=== Phase 1: Compile software ==="
cd $TEST_NAME && make clean rv32im && cd ..

echo "=== Phase 2: Run simulation ==="
sbt "runMain vexiiriscv.execute.Vexii${PLUGIN_NAME}Sim \
  --load-elf $ELF_PATH \
  --trace-all \
  --no-rvls-check"

echo "=== Phase 3: Check results ==="
echo "Waveform: simWorkspace/VexiiRiscv/test/wave.fst"
echo "Pipeline: simWorkspace/VexiiRiscv/test/konata.log"
```

Make it executable and run:
```bash
chmod +x scripts/test_custom.sh
./scripts/test_custom.sh
```

## Key Files Reference

| Purpose | File |
|---------|------|
| Hardware plugin | `src/main/scala/vexiiriscv/execute/<PluginName>.scala` |
| Generator App | Same file - `object Vexii<Name>Gen extends App` |
| Simulator App | Same file - `object Vexii<Name>Sim extends App` |
| Software test | `mytest/src/test.S` |
| Makefile | `mytest/Makefile` |
| Driver headers | `sw/driver/*.h` |

## Command Reference

### Generate Hardware
```bash
sbt "runMain vexiiriscv.execute.Vexii<Name>Gen"
```

### Run Simulation
```bash
sbt "runMain vexiiriscv.execute.Vexii<Name>Sim --load-elf <elf> --trace-all --no-rvls-check"
```

### Compile Software
```bash
cd mytest && make clean rv32im
```

## Success Criteria

1. ✅ Hardware generates without errors
2. ✅ ELF compiles successfully
3. ✅ Simulation runs to completion
4. ✅ Test reaches pass label (or expected output)
5. ✅ Waveform shows correct signal behavior
