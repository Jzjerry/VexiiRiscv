# Compile RISC-V Software for VexiiRiscv

Use the sw/ build system to compile RISC-V ELF executables.

## Trigger Words
- compile riscv
- build elf
- riscv gcc
- create firmware

## Quick Start

Navigate to the sw directory and compile:
```bash
cd sw
make
```

This compiles the default application and produces:
- `build/sw.elf` - ELF executable
- `build/sw.bin` - Raw binary
- `build/sw.asm` - Disassembly
- `build/sw.map` - Memory map

## Project Structure

```
sw/
├── Makefile              # Main build file
├── src/
│   ├── start.S           # Startup/boot code
│   └── main.c            # Your application
├── driver/               # Hardware drivers & headers
│   ├── sim.h             # Simulation I/O (sim_putchar, etc.)
│   ├── riscv.h           # RISC-V CSR macros
│   ├── riscv_asm.h       # Assembly ├── custom_asm macros
│  .h      # Custom instruction macros
│   └── ...
├── common/
│   ├── app.ld            # Linker script
│   └── ...
└── build/                # Output directory
```

## Create Your Own Project

### 1. Create project directory
```bash
mkdir -p myproject/src
```

### 2. Write startup code (src/start.S)
```asm
.option arch, +zicsr

.global _start
_start:
    # Your initialization code

pass:
    j pass   # Infinite loop on success
fail:
    j fail   # Infinite loop on failure
```

### 3. Write main application (src/main.c)
```c
#include <sim.h>

void main() {
    sim_putchar('H');
    sim_putchar('e');
    sim_putchar('l');
    sim_putchar('l');
    sim_putchar('o');
    sim_putchar('\n');
}
```

### 4. Create Makefile
```makefile
PROJ_NAME=myproject
STANDALONE=../ext/NaxSoftware/baremetal
SRCS =  $(wildcard src/*.c) \
        $(wildcard src/*.cpp) \
        $(wildcard src/*.S) \
        ${STANDALONE}/common/start.S
include ${STANDALONE}/common/app.mk
```

### 5. Compile and run
```bash
make
# Output: build/myproject.elf

# Run in simulation
cd ..
sbt "Test/runMain vexiiriscv.tester.TestBench --load-elf sw/build/myproject.elf"
```

## Available Headers

| Header | Description |
|--------|-------------|
| `<sim.h>` | Simulation I/O: `sim_putchar()`, `sim_puthex()` |
| `<riscv.h>` | CSR access: `csr_read()`, `csr_write()` |
| `<riscv_asm.h>` | Inline assembly helpers |
| `<custom_asm.h>` | Custom instruction macros |
| `<privileged.h>` | Privilege mode macros |
| `<driver/bsp.h>` | Board support package |

## Assembly Programming

### Using RISC-V assembly
```asm
.option arch, +zicsr

.global _start
_start:
    li x1, 42       # Load immediate
    li x2, 100
    add x3, x1, x2  # x3 = x1 + x2

pass:
    j pass
```

### Using custom instructions
```asm
#include "../../driver/custom_asm.h"

# Custom0 instruction (opcode 0x0B)
# opcode_R(CUSTOM0, func3, func7, rd, rs1, rs2)
opcode_R(CUSTOM0, 0x0, 0x00, x3, x1, x2)  # x3 = custom_op(x1, x2)
```

## Build Options

Build for RV32 (default):
```bash
make clean rv32im
```

Build for RV64:
```bash
make clean rv64imafdc
```

Clean build:
```bash
make clean
make
```

## Environment Variables

Set custom RISC-V toolchain path:
```bash
export RISCV=/opt/riscv
make
```
