# Simulate VexiiRiscv with ELF Files

Run Verilator-based simulation loading RISC-V ELF executables.

## Trigger Words
- simulate vexii
- run simulation
- load elf
- verilator simulation

## Basic Usage

Run simulation with an ELF file:
```bash
sbt "Test/runMain vexiiriscv.tester.TestBench --load-elf <path-to-elf>"
```

## Common Options

| Option | Description |
|--------|-------------|
| `--load-elf <path>` | Load ELF file into memory before simulation |
| `--trace-all` | Enable all traces (waveform, pipeline, etc.) |
| `--no-rvls-check` | Disable RVLS/Spike golden model checking |
| `--no-probe` | Disable CPU inactivity watchdog |
| `--with-mul`, `--with-div` | Enable mul/div instructions |
| `--with-btb`, `--with-gshare`, `--with-ras` | Enable branch prediction |
| `--allow-bypass-from=0` | Enable result forwarding for better IPC |
| `--debug-privileged` | Enable privileged mode debug CSR |
| `--debug-jtag-tap` | Enable JTAG debug interface |
| `--jtag-remote` | Enable TCP-to-JTAG bridge for OpenOCD |

## Simulation Output

After running, check `simWorkspace/VexiiRiscv/test/`:

- `test.fst` / `wave.fst` - Waveform file (open with gtkwave)
- `konata.log` - Pipeline visualization (open with Konata)
- `spike.log` - Spike golden model execution log
- `tracer.log` - VexiiRiscv execution log

## Examples

Basic simulation:
```bash
sbt "Test/runMain vexiiriscv.tester.TestBench --load-elf sw/build/sw.elf --trace-all"
```

With mul/div and full branch prediction:
```bash
sbt "Test/runMain vexiiriscv.tester.TestBench --with-mul --with-div --with-btb --with-gshare --with-ras --load-elf mytest.elf --trace-all"
```

With custom instruction (no RVLS check):
```bash
sbt "Test/runMain vexiiriscv.tester.TestBench --load-elf custom.elf --trace-all --no-rvls-check"
```

## Debugging with OpenOCD

Start simulation with JTAG:
```bash
sbt "Test/runMain vexiiriscv.tester.TestBench --load-elf test.elf --no-probe --no-rvls-check --debug-privileged --debug-jtag-tap --jtag-remote"
```

Then connect OpenOCD:
```bash
cd src/main/tcl/openocd/ && openocd -f vexiiriscv_sim.tcl
```

Connect via telnet for debugging:
```bash
telnet localhost 4444
# Then use commands like: mdw 0x80000000, reg pc, step
```

## Performance Tips

- Use SBT shell for faster repeated runs: `sbt` then `Test/runMain ...`
- Use `--allow-bypass-from=0` for better IPC
- Use `--no-rvls-check` when testing custom instructions not in Spike
