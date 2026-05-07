# stm32-hal-blink-minimal

Clone from https://github.com/UtkarshVerma/stm32-hal-blink-minimal
This test for quick build and testing seer using stm32 smart v2.0j board, or essentially, you can use any stm32f1 board

# Hardware

+ stm32 smart v2.0j board
+ J-Link, or you can use other hardware debugger if you're capable of doing

# Prerequisite

+ Install docker, makefile
+ Install openocd: `./install_openocd.sh`

# Instruction

+ Build STM32 firmware: (run in docker container)
+ Build docker image: `make build_docker`
+ Run docker image: `make docker`
+ Build image in docker: `./build.sh` <br>

Final image is located at: stm32f1_blink/build/main.elf<br>

+ Flash image to STM32 board: (Run in host terminal. Not in docker.)
+ make flash JLINK_CFG=interface/jlink.cfg <br>

You'll see the STM32 led blinks

## To launch openocd as a 'gdbserver' session connected to the dev board. (In host terminal.)

```
$ openocd -f interface/jlink.cfg -f target/stm32f1x.cfg
```

## To launch Seer connected to the openocd 'gdbserver'. (In a second host terminal.)

```
$ seergdb --gdb-program=gdb-multiarch -s --connect=:3333 build/main.elf
```

## To launch gdb connected to the openocd 'gdbserver'. (In a second host terminal.)

```
$ gdb-multiarch build/main.elf
(gdb) target extended-remote :3333
Remote debugging using :3333
main () at src/main.c:50
50          while (1) {
(gdb)
```

# Troubleshoot:

+ Install Jlink driver
+ Proper connection
+ Modify build.sh: JLINK_CFG vatiable to choose appropriate hardware debugger. Modify it accordingly by specify pat to jlink.cfg file of openocd

# Contact

If you're having any trouble about flashing or building image, please contact me via email: nguyenminhquangcn1@gmail.com

