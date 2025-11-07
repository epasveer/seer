# Rpi4
This example is for testing OpenOCD capablility of debugging Linux kernel and kerel module.<br>
Debugging hardware will be more laborious than debugging PC application.
# Prerequisite
+ Hardware debugger: JLink, Olimex, .... any hardware debugger supported by OpenOCD. In this case, Jlink has been used.
+ Rasberry Pi 4, micros SD card, micros SD card reader
+ OpenOCD installed on your system. You can run `install_openocd.sh` to automatically install it.
+ Docker installed on your system. You can run `install_docker.sh` to automatically install it.
# Build Rpi4 image
+ `make build_docker` to build docker
+ `make docker` to enter docker container
+ `./build_Rpi4_owrt.sh` inside docker to automatically build image for Rpi4. Wait for 1 or 2 hours, depends on your internet speed and PC specs
+ Final image will be: openwrt-bcm27xx-bcm2711-rpi-4-squashfs-factory.img
# Flash image to your microSD card
Please read this carefully before deployment
+ Run following command in other terminal (not in docker)
+ Identify microSD card partition: `lsblk`, assume microSD card partition is: /dev/sdX
+ Unmount any partition from microSD `umount /dev/sdX`
+ Flash Rpi4 image, modify path to Rpi4 image and microSD card partition sdX:
```
sudo dd if=openwrt-bcm27xx-bcm2711-rpi-4-squashfs-factory.img of=/dev/sdX bs=4M status=progress
```
+ Sync and eject
```
sync
sudo eject /dev/sdX
```
# Hardware connection
Check Rpi4 pin out here (Alt4): https://pinout.xyz/pinout/jtag<br>

|   Pin name    |   Jtag connector  |     Rpi4          |
|---------------|-------------------|-------------------|
|   VCC         |       2           |       3v3 power   |
|   TRST        |       3           |       GPIO 22     |
|   TDI         |       5           |       GPIO 26     |
|   TMS         |       7           |       GPIO 27     |
|   TCLK        |       9           |       GPIO 25     |
|   RTCK        |       11          |       GPIO 23     |
|   TDO         |       13          |       GPIO 24     |
|   GND         |       18          |       Any Ground  |