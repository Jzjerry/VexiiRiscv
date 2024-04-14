PROJ_NAME = IceSoc
RTL_NAME = IceSoc

BUILD_DIR = build
RTL_DIR = hw/rtl
CONSTR_DIR = hw/constr
LOG_DIR = logs

RTL = $(wildcard ${RTL_DIR}/*.v)
CONSTR = ${CONSTR_DIR}/${PROJ_NAME}.pcf
BIN = ${BUILD_DIR}/${PROJ_NAME}.bin
TARGET = ${BUILD_DIR}/${PROJ_NAME}
PNRFLAGS = --top ${RTL_NAME} --package sg48 --report ${LOG_DIR}/pnr_report.json
PNRFLAGS += --routed-svg ${LOG_DIR}/route.svg

SYNTHFLAGS = -dsp -abc2

VERBOSE ?=

ifeq (${VERBOSE}, yes)
	output = | tee
	PNRFLAGS += -v --log ${LOG_DIR}/pnr.log
else
	ifeq (${VERBOSE}, no_log)
		output = ; touch
		PNRFLAGS += -q
	else
		output = >
		PNRFLAGS += -q --log ${LOG_DIR}/pnr.log
	endif
endif

all: mkdir compile

rtl:
	sbt "runMain vexiiriscv.soc.icesoc.IceSocGen" ${output} ${LOG_DIR}/spinal.log

mkdir:
	@mkdir -p ${BUILD_DIR} ${LOG_DIR}

synth: ${TARGET}.asc

time: ${TARGET}.bin
	icetime -tmd up5k ${TARGET}.asc

compile: ${TARGET}.bin

clean:
	rm -rf ${BUILD_DIR} ${RTL}

${RTL}: rtl
	@echo "RTL Generated: $@"

${TARGET}.json: ${RTL}
	@echo "Synthesizing..."
	yosys -p "synth_ice40 ${SYNTHFLAGS} -top ${RTL_NAME} -json $@" $^ ${output} ${LOG_DIR}/yosys.log

${TARGET}.asc: ${CONSTR} ${TARGET}.json
	@echo "Place&Route ing..."
	nextpnr-ice40 ${PNRFLAGS} --up5k  --pcf $< --json $(filter-out $<,$^) --asc $@ 

${TARGET}.bin: ${TARGET}.asc
	@echo "Compiling $@"
	icepack $^ $@

.PHONY: rtl mkdir synth clean compile time