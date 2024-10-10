sbt "runMain vexiiriscv.tester.TestBench \
    --with-rvc \
    --with-mul \
    --with-div \
    --performance-counters 4 \
    --load-elf ../../sw_riscv/build/mico.elf \
    --no-rvls-check \
    --print-stats"