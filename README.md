# mini-RISC

A minimal 16-bit RISC CPU written in VHDL. There is no real world purpose for this project other than educational purposes, yet.

The CPU features 16 16-bit wide registers r0 to r15 and 16-bit wide instructions with 16 different opcodes running from ROM. It supports memory mapped I/O for accessing GPIOs and other peripherals. A more detailed description of the ISA can be found in the source code.

This project also includes a very simple assembler for easier code writing and generating machine code.

## Running

The CPU was developed and tested with GHDL. Compile the sources and testbench using:
```
ghdl -a  *.vhd && ghdl -e testbench
```

Then run the testbench:
```
./testbench --wave=waves.ghw
```

The resulting waveforms can be viewed using GTKWave:
```
gtkwave waves.ghw waves.gtkw &
```

The testbench runs a simple program which multiplies two numbers from registers r0 and r1.

## Using the assembler

Sources for the assembler are located in the subdirectory `asm/` and can be compiled using `make`.
