# VexiiRiscv Parameters Reference

Complete reference for VexiiRiscv generation and simulation parameters.

## Trigger Words
- vexii parameters
- vexii options
- generation options
- cpu configuration

## ISA Extensions

| Parameter | Description | Notes |
|-----------|-------------|-------|
| `--xlen=32/64` | Data width | RV32 or RV64 |
| `--with-rvm` | Mul/Div (M extension) | Integer multiply/divide |
| `--with-rva` | Atomic (A extension) | Atomic operations |
| `--with-rvc` | Compressed (C extension) | 16-bit compressed instructions |
| `--with-rvf` | Float32 (F extension) | Single-precision FP |
| `--with-rvd` | Float64 (D extension) | Double-precision FP |
| `--with-rvs` | Supervisor (S extension) | Supervisor mode + MMU |
| `--with-rvu` | User (U extension) | User mode |

## Pipeline & Performance

| Parameter | Description |
|-----------|-------------|
| `--decoders <n>` | Number of instruction decoders |
| `--lanes <n>` | Number of execution lanes |
| `--relaxed-branch` | Allow branch in late pipeline |
| `--relaxed-shift` | Allow shift in late pipeline |
| `--relaxed-src` | Allow src read in late pipeline |
| `--regfile-async` | Async register file reads (save 1 stage) |
| `--mmu-sync-read` | Sync TLB reads for FPGA |
| `--allow-bypass-from=<stage>` | Result forwarding from stage N |
| `--div-radix <n>` | Division radix (2 or 4) |

## Branch Prediction

| Parameter | Description |
|-----------|-------------|
| `--with-btb` | Branch Target Buffer |
| `--with-gshare` | GShare conditional branch predictor |
| `--with-ras` | Return Address Stack |
| Note: BTB required for gshare and ras |

## Caches

| Parameter | Description |
|-----------|-------------|
| `--fetch-l1` | Enable L1 instruction cache |
| `--fetch-l1-ways=<n>` | Number of I$ ways |
| `--fetch-l1-bytes=<n>` | I$ KB per way |
| `--lsu-l1` | Enable L1 data cache |
| `--lsu-l1-ways=<n>` | Number of D$ ways |
| `--lsu-l1-bytes=<n>` | D$ KB per way |

## Memory & Debug

| Parameter | Description |
|-----------|-------------|
| `--with-boot-mem-init` | Initialize memory from ELF at reset |
| `--with-jtag-tap` | JTAG debugging interface |
| `--with-mmu` | Memory Management Unit |
| `--report-model` | Print pipeline model after generation |

## Other Options

| Parameter | Description |
|-----------|-------------|
| `--with-pmp` | Physical Memory Protection |
| `--performance-counters=<n>` | Enable performance counters |
| `--with-dsp` | DSP extensions |

## Quick Configurations

Minimal RV32I:
```bash
sbt "Test/runMain vexiiriscv.Generate"
```

High-performance RV32GC:
```bash
sbt "Test/runMain vexiiriscv.Generate --decoders 2 --lanes 2 --with-btb --with-gshare --with-ras --with-rvc --with-rvf --with-rvd --with-rvm --allow-bypass-from=0 --regfile-async"
```

Linux-capable RV64GC:
```bash
sbt "Test/runMain vexiiriscv.Generate --xlen=64 --with-rvc --with-rvf --with-rvd --with-rvm --with-supervisor --fetch-l1 --lsu-l1"
```

MCU (minimal area):
```bash
sbt "Test/runMain vexiiriscv.Generate --xlen=32"
```

## Get Help

List all available parameters:
```bash
sbt "Test/runMain vexiiriscv.Generate --help"
```
