# Rpi4
This example is for testing OpenOCD capablility of debugging Linux kernel and kerel module.<br>
# Prerequisite
+ Hardware debugger: JLink, Olimex, .... any hardware debugger supported by OpenOCD. In this case, Jlink has been used.
+ Rasberry Pi 4, micros SD card, micros SD card reader
+ OpenOCD installed on your system. You can run `install_openocd.sh` to automatically install it.
+ Docker installed on your system. You can run `install_docker.sh` to automatically install it.
# Build Rpi4 image
+ `make build_docker` to build docker
+ `make docker` to enter docker container
+ `make owrt` to build owrt image. Final image will be: openwrt-bcm27xx-bcm2711-rpi-4-squashfs-factory.img
+ `make yocto` to build yocto image. Final image will be: temp.wic
# Flash image to your microSD card
Please read this carefully before deployment
+ Run following command in other terminal (not in docker)
+ Identify microSD card partition: `lsblk`, assume microSD card partition is: /dev/sdX
+ Unmount any partition from microSD `umount /dev/sdX`
+ Flash Rpi4 image, modify path to Rpi4 image and microSD card partition sdX:
```
sudo dd if=openwrt-bcm27xx-bcm2711-rpi-4-squashfs-factory.img of=/dev/sdX bs=4M status=progress
```
or
```
sudo dd if=temp.wic of=/dev/sdX bs=4M status=progress
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

# Userspace debugging with gdbserver + Seer

The image ships `gdbserver` and the `recursive-test` package, which installs
`/usr/bin/math_toolkit` (the `../recursive_test` C++ program). It is built
**statically, non-PIE, `-g -O0`, unstripped** on purpose: with no shared
libraries and a fixed load address, remote debugging needs no sysroot, no
shared-library relocation, and behaves identically in launch and `--attach`
modes - which sidesteps the GDB-15-host / gdbserver-12-target `lmid`
incompatibility in the svr4 library list.

After a build, `debug-rpi4/` holds the matching unstripped binary plus the
exact source tree the binary was compiled from. The on-Pi
`/usr/bin/math_toolkit` and `debug-rpi4/math_toolkit` are byte-identical
(check with `sha256sum`) - keep them in sync after every rebuild.

### On the Raspberry Pi

Connect the Pi's Ethernet and reach it over the network (see the IP section
above; default LAN is `192.168.1.1`). Then, launch mode:

```
ssh root@<pi-ip>
gdbserver --once :2345 /usr/bin/math_toolkit
```

or attach mode (program already running):

```
math_toolkit &
gdbserver --once --attach :2345 $(pidof math_toolkit)
```

`--once` makes gdbserver exit cleanly when the session ends; re-run it before
each new session.

### On the host

```
seergdb --gdb-program gdb-multiarch \
        --connect <pi-ip>:2345 \
        --sym  /abs/path/tests/Rpi4/debug-rpi4/math_toolkit \
               /abs/path/tests/Rpi4/debug-rpi4/math_toolkit
```

In Seer's gdb command list (Settings -> Configuration) or `~/.gdbinit`, before
connecting:

```
set architecture aarch64
set remotetimeout 60
set mi-async on
set non-stop off
set substitute-path /workdir/openwrt/build_dir/target-aarch64_cortex-a72_musl/recursive-test-1.0 /abs/path/tests/Rpi4/debug-rpi4
```

Set a breakpoint (`break factorial(int)`, `break main`), press **Continue**
(not Interrupt - Interrupt only does something once the program is running).
`math_toolkit` loops every 2 s, so breakpoints in `factorial` / `fibonacci`
hit repeatedly.
