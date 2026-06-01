#!/bin/bash
# Build script for Yocto Raspberry Pi 4

LOGFILE=build.log

exec > >(tee -a ${LOGFILE}) 2>&1

########################################################################################################################
# Configuration
########################################################################################################################
YOCTO_DIR=$PWD/yocto-rpi
POKY_REPO="https://git.yoctoproject.org/poky"
POKY_BRANCH="kirkstone"

META_RPI_REPO="https://github.com/agherzan/meta-raspberrypi.git"
META_RPI_BRANCH="kirkstone"

BUILD_DIR=build
MACHINE="raspberrypi4"
IMAGE="core-image-minimal"

META_PI=$YOCTO_DIR/meta-raspberrypi
ROOT_DIR=$PWD

########################################################################################################################
# Functions
########################################################################################################################


echo_red() {
    echo -e "\033[31m\033[1m$1\033[0m"
}

echo_green() {
    echo -e "\033[32m\033[1m$1\033[0m"
}

echo_blue() {
    echo -e "\033[34m\033[1m$1\033[0m"
}

run_step() {
    echo "======================================"
    echo_blue "Running: $1"
    echo "======================================"
    eval "$1"
}

check_error() {
    if [ $? -ne 0 ]; then
        echo_red "ERROR: Step failed!"
        exit 1
    fi
}

########################################################################################################################
# Start of code
########################################################################################################################
echo_blue "===== YOCTO RPI4 BUILD START ====="

# Clone poky
if [ ! -d "$YOCTO_DIR/poky" ]; then
    run_step "git clone -b $POKY_BRANCH $POKY_REPO $YOCTO_DIR/poky"
fi

cd $YOCTO_DIR

# Clone meta-raspberrypi
if [ ! -d "meta-raspberrypi" ]; then
    run_step "git clone -b $META_RPI_BRANCH $META_RPI_REPO"
fi

# Clone meta-openembedded (provides bridge-utils in meta-networking)
if [ ! -d "meta-openembedded" ]; then
    run_step "git clone -b $POKY_BRANCH https://github.com/openembedded/meta-openembedded.git"
fi

# Init build env
cd poky
run_step "source oe-init-build-env $BUILD_DIR"

# run_step "bitbake -c clean"

# Add layers
run_step "bitbake-layers add-layer ../../meta-raspberrypi"
# Add meta-networking from meta-openembedded so bridge-utils is available
run_step "bitbake-layers add-layer ../../meta-openembedded/meta-oe"
run_step "bitbake-layers add-layer ../../meta-openembedded/meta-python"
run_step "bitbake-layers add-layer ../../meta-openembedded/meta-networking"

# Configure local.conf
CONF_FILE="conf/local.conf"

echo_blue "Configuring local.conf..."

# Set machine
grep -q "^MACHINE" $CONF_FILE && \
    sed -i "s/^MACHINE.*/MACHINE = \"$MACHINE\"/" $CONF_FILE || \
    echo "MACHINE = \"$MACHINE\"" >> $CONF_FILE

# Enable U-Boot
cat <<EOF >> $CONF_FILE

# Use U-Boot instead of default firmware boot
RPI_USE_U_BOOT = "1"
ENABLE_UART = "1"
ENABLE_JTAG = "1"
# Include kernel modules (and specific module if you want)
IMAGE_INSTALL:append = " xz python3 bridge-utils gcc usbutils git wget iproute2 gdbserver kernel-module-spi-bcm2835aux kernel-module-enc28j60 helloworld myplatform dtc"
KERNEL_MODULE_AUTOLOAD += "spi-bcm2835aux"

# Optional: ensure firmware if needed
IMAGE_INSTALL:append = " linux-firmware"
EOF

# Optional: speed up build
cat <<EOF >> $CONF_FILE

BB_NUMBER_THREADS ?= "$(nproc)"
PARALLEL_MAKE ?= "-j$(nproc)"

EOF

########################################################################################################################
# Customization, apply patches
########################################################################################################################
pushd $META_PI
# Enable JTAG, kgdb, debug symbol to rpi4 config file
mkdir -p recipes-kernel/linux/files
touch recipes-kernel/linux/files/debug.cfg
cat <<EOF > recipes-kernel/linux/files/debug.cfg
# Kernel hacking
CONFIG_DEBUG_INFO=y
CONFIG_DEBUG_INFO_DWARF_TOOLCHAIN_DEFAULT=y
CONFIG_GDB_SCRIPTS=y

# KGDB / KDB
CONFIG_KGDB=y
CONFIG_KGDBOE=y
CONFIG_KGDB_SERIAL_CONSOLE=y
CONFIG_KGDB_KDB=y
CONFIG_KDB_KEYBOARD=y
CONFIG_KDB_CONTINUE_ON_CONSOLE=y

# Other useful debugging options
CONFIG_MAGIC_SYSRQ=y
CONFIG_MAGIC_SYSRQ_DEFAULT_ENABLE=0x1ff
CONFIG_FRAME_POINTER=y
CONFIG_DEBUG_KERNEL=y
CONFIG_DEBUG_MISC=y
# CONFIG_DEBUG_RODATA is not set
CONFIG_NETPOLL=y
CONFIG_KALLSYMS

EOF

# ENC28J60 driver as a loadable kernel module
cat <<EOF > recipes-kernel/linux/files/enc28j60.cfg
CONFIG_ENC28J60=m
EOF

# Append to kernel recipe (linux-raspberrypi) via bbappend
cat <<'EOF' > recipes-kernel/linux/linux-raspberrypi_%.bbappend

FILESEXTRAPATHS:prepend := "${THISDIR}/files:"

SRC_URI += "file://debug.cfg file://enc28j60.cfg file://bcm2711-rpi-4-b.dts"

KERNEL_CONFIG_FRAGMENTS += "${WORKDIR}/debug.cfg ${WORKDIR}/enc28j60.cfg"

do_patch:append() {
    cp ${WORKDIR}/bcm2711-rpi-4-b.dts ${S}/arch/arm/boot/dts/bcm2711-rpi-4-b.dts
}

EOF



# Enable JTAG debug and KGDB on uboot
cp -rf $ROOT_DIR/patches/rpi-config_git.bb $META_PI/recipes-bsp/bootfiles
cp -rf $ROOT_DIR/patches/rpi-cmdline.bb $META_PI/recipes-bsp/bootfiles

# Install helloworld kernel module recipe
mkdir -p $META_PI/recipes-kernel/helloworld/files
cp -f $ROOT_DIR/helloworld/helloworld.c $META_PI/recipes-kernel/helloworld/files/
cp -f $ROOT_DIR/helloworld/Makefile $META_PI/recipes-kernel/helloworld/files/
cp -f $ROOT_DIR/patches/helloworld.bb $META_PI/recipes-kernel/helloworld/

# Install myplatform kernel module recipe
mkdir -p $META_PI/recipes-kernel/myplatform/files
cp -f $ROOT_DIR/myplatform/myplatform.c $META_PI/recipes-kernel/myplatform/files/
cp -f $ROOT_DIR/myplatform/Makefile $META_PI/recipes-kernel/myplatform/files/
cp -f $ROOT_DIR/patches/myplatform.bb $META_PI/recipes-kernel/myplatform/

# Stage modified device tree for the kernel bbappend
cp -f $ROOT_DIR/patches/bcm2711-rpi-4-b.dts $META_PI/recipes-kernel/linux/files/
popd
########################################################################################################################
# Build
########################################################################################################################
run_step "bitbake $IMAGE"
if [ $? -ne 0 ]; then
    echo_red "ERROR: Bitbake failed!"
    exit 1
fi

echo_green "Build successful!"

cd $ROOT_DIR
cp -rf $ROOT_DIR/yocto-rpi/poky/build/tmp/deploy/images/raspberrypi4/core-image-minimal-raspberrypi4-*.rootfs.wic.bz2 temp.wic.bz2
bunzip2 -f temp.wic.bz2
########################################################################################################################
# Output
########################################################################################################################
echo_blue "===== BUILD COMPLETE ====="
echo_blue "Image located at: temp.wic"
echo_blue "Now run:"
echo_blue "sudo dd if=temp.wic of=/dev/sdX bs=4M status=progress"
echo_blue "sync and eject"
