# IceSoc

A simple SoC with VexiiRiscv. 
Just big enough to exploit all resources on ice40up5k.

## Synthese Results
`synth_ice40 -dsp` with 2 SPRAMs manually blackboxed:
```
Info: Device utilisation:
Info: 	         ICESTORM_LC:  4853/ 5280    91%
Info: 	        ICESTORM_RAM:    16/   30    53%
Info: 	               SB_IO:     4/   96     4%
Info: 	               SB_GB:     8/    8   100%
Info: 	        ICESTORM_PLL:     0/    1     0%
Info: 	         SB_WARMBOOT:     0/    1     0%
Info: 	        ICESTORM_DSP:     6/    8    75%
Info: 	      ICESTORM_HFOSC:     0/    1     0%
Info: 	      ICESTORM_LFOSC:     0/    1     0%
Info: 	              SB_I2C:     0/    2     0%
Info: 	              SB_SPI:     0/    2     0%
Info: 	              IO_I3C:     0/    2     0%
Info: 	         SB_LEDDA_IP:     0/    1     0%
Info: 	         SB_RGBA_DRV:     0/    1     0%
Info: 	      ICESTORM_SPRAM:     2/    4    50%

Fmax = 20.16 MHz
```

`synth_ice40 -dsp -abc2` with 2 SPRAMs manually blackboxed:
```
Info: Device utilisation:
Info: 	         ICESTORM_LC:  4857/ 5280    91%
Info: 	        ICESTORM_RAM:    16/   30    53%
Info: 	               SB_IO:     4/   96     4%
Info: 	               SB_GB:     8/    8   100%
Info: 	        ICESTORM_PLL:     0/    1     0%
Info: 	         SB_WARMBOOT:     0/    1     0%
Info: 	        ICESTORM_DSP:     6/    8    75%
Info: 	      ICESTORM_HFOSC:     0/    1     0%
Info: 	      ICESTORM_LFOSC:     0/    1     0%
Info: 	              SB_I2C:     0/    2     0%
Info: 	              SB_SPI:     0/    2     0%
Info: 	              IO_I3C:     0/    2     0%
Info: 	         SB_LEDDA_IP:     0/    1     0%
Info: 	         SB_RGBA_DRV:     0/    1     0%
Info: 	      ICESTORM_SPRAM:     2/    4    50%

Fmax = 20.31 MHz
```

`synth_ice40 -dsp -flowmap` with 2 SPRAMs manually blackboxed: Failed, exceeded LC limitation.

`synth_ice40 -dsp -abc9` with 2 SPRAMs manually blackboxed:
```
Info: Device utilisation:
Info: 	         ICESTORM_LC:  4935/ 5280    93%
Info: 	        ICESTORM_RAM:    16/   30    53%
Info: 	               SB_IO:     4/   96     4%
Info: 	               SB_GB:     8/    8   100%
Info: 	        ICESTORM_PLL:     0/    1     0%
Info: 	         SB_WARMBOOT:     0/    1     0%
Info: 	        ICESTORM_DSP:     6/    8    75%
Info: 	      ICESTORM_HFOSC:     0/    1     0%
Info: 	      ICESTORM_LFOSC:     0/    1     0%
Info: 	              SB_I2C:     0/    2     0%
Info: 	              SB_SPI:     0/    2     0%
Info: 	              IO_I3C:     0/    2     0%
Info: 	         SB_LEDDA_IP:     0/    1     0%
Info: 	         SB_RGBA_DRV:     0/    1     0%
Info: 	      ICESTORM_SPRAM:     2/    4    50%

Fmax = 18.69 MHz
```

`synth_ice40 -dsp -spram`: Failed, couldn't build a 2*16-bit SPRAM for the 32-bit RAM.

