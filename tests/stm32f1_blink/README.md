# stm32-hal-blink-minimal
Clone from https://github.com/UtkarshVerma/stm32-hal-blink-minimal
This test for quick build and testing seer using stm32 smart v2.0j board, or essentially, you can use any stm32f1 board
# Hardware
+ stm32 smart v2.0j board
+ J-Link, or you can use other hardware debugger if you're capable of doing
# Prerequisite
Install gdb-multiarch:
```sudo apt install gcc-arm-none-eabi```
and install openocd and openocd driver for J-link:
```./install_openocd.sh```
# Build 
Connect hardware debugger to stm32f1 board
run `./build.sh`, it will pull remote library, build and flash image to stm32f1 automatically for your
# Troubleshoot:
+ Install Jlink driver
+ Proper connection
+ Modify build.sh: JLINK_CFG vatiable to choose appropriate hardware debugger. Modify it accordingly by specify pat to jlink.cfg file of openocd
# Contact
If you're having any trouble about flashing or building image, please contact me via email: nguyenminhquangcn1@gmail.com