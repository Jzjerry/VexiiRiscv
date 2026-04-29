# Generate VexiiRiscv Hardware

Generate Verilog hardware from VexiiRiscv using SBT and SpinalHDL.

## Trigger Words
- generate hardware
- generate verilog
- build vexii riscv
- create vexii design

## Usage

Generate default RV32I VexiiRiscv:
```bash
sbt "Test/runMain vexiiriscv.Generate"
```

This generates `VexiiRiscv.v` in the project root.

## Common Parameters

| Parameter | Description |
|-----------|-------------|
| `--xlen=32/64` | CPU data width (default: 32) |
| `--with-rvm` | Enable RISC-V mul/div instructions |
| `--with-rvc` | Enable compressed instruction set (RVC) |
| `--with-rva` | Enable atomic instructions |
| `--with-rvf` | Enable 32-bit floating point |
| `--with-rvd` | Enable 64-bit floating point |
| `--with-supervisor` | Enable supervisor mode + MMU |
| `--with-btb` | Enable Branch Target Buffer |
| `--with-gshare` | Enable GShare branch prediction |
| `--with-ras` | Enable Return Address Stack |
| `--fetch-l1` | Enable L1 instruction cache |
| `--lsu-l1` | Enable L1 data cache |
| `--with-jtag-tap` | Enable JTAG debugging |

## Examples

Generate RV32IM (with mul/div):
```bash
sbt "Test/runMain vexiiriscv.Generate --xlen=32 --with-rvm"
```

Generate RV64GC (64-bit with compressed + floating point):
```bash
sbt "Test/runMain vexiiriscv.Generate --xlen=64 --with-rvc --with-rvf --with-rvd"
```

Generate with branch prediction and caches:
```bash
sbt "Test/runMain vexiiriscv.Generate --with-btb --with-gshare --with-ras --fetch-l1 --lsu-l1"
```

Get all available parameters:
```bash
sbt "Test/runMain vexiiriscv.Generate --help"
```

## Output Files

- `VexiiRiscv.v` - Generated Verilog file
- Hardware is ready for FPGA synthesis (Quartus, Vivado, etc.)
