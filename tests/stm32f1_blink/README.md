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
+ Flash image to STM32 board: (run in host terminal)
	+ make flash JLINK_CFG=interface/jlink.cfg <br>
You'll see the STM32 led blinks
# Troubleshoot:
+ Install Jlink driver
+ Proper connection
+ Modify build.sh: JLINK_CFG vatiable to choose appropriate hardware debugger. Modify it accordingly by specify pat to jlink.cfg file of openocd
# Contact
If you're having any trouble about flashing or building image, please contact me via email: nguyenminhquangcn1@gmail.com