#!/bin/bash

# Script to build OpenWrt image for Raspberry Pi 4 with debug symbols
# Assumes OpenWrt build environment is set up (docker)
# I have tested with kernel version 6.15 but vmlinux symbol file is optimized out so feature Debug on init can't be 
# performed with kernel v6.15
# It is recommended to use Linux kernel version 5.15

# Exit on error
set -x
########################################################################################################################
# Variable
########################################################################################################################
git config --global http.postBuffer 524288000
git config --global http.maxRequestBuffer 100M
git config --global http.version HTTP/1.1
git config --global http.maxRequests 100
git config --global core.compression 0

# Define variables
TOPDIR=$PWD
OPENWRT_DIR="$PWD/openwrt"
TARGET="bcm27xx/bcm2711"
KERNEL_CONFIG="$OPENWRT_DIR/target/linux/$TARGET/config-5.15"  # Adjust kernel version if needed
UBOOT_DIR="$OPENWRT_DIR/bootloaders/u-boot"
UBOOT_CONFIG="$UBOOT_DIR/configs/rpi_4_defconfig"
MAKE_JOBS=$(nproc)  # Number of CPU cores for parallel build
HELLO_WORLD_DIR="$OPENWRT_DIR/package/kernel"
########################################################################################################################
# Functions
########################################################################################################################
echo_green() {
    echo -e "\033[32m$1\033[0m"
}

# Function to print text in red
echo_red() {
    echo -e "\033[31m$1\033[0m"
}
########################################################################################################################
# Build
########################################################################################################################
echo "Cloning OpenWrt repository..."
git clone --branch v23.05.0-rc4 --depth=1 https://github.com/openwrt/openwrt.git
cd $OPENWRT_DIR
# Step 2: Update feeds
echo "Updating feeds..."
./scripts/feeds update -a
./scripts/feeds install -a

# Step 3: Configure OpenWrt for Raspberry Pi 4
echo "Configuring OpenWrt for Raspberry Pi 4..."
make defconfig
cat << EOF > .config
CONFIG_TARGET_bcm27xx=y
CONFIG_TARGET_bcm27xx_bcm2711=y
CONFIG_TARGET_bcm27xx_bcm2711_DEVICE_rpi-4=y
CONFIG_DEBUG_INFO=y
CONFIG_EXPERT=y
CONFIG_KALLSYMS=y
CONFIG_SYSFS=y
CONFIG_PID_IN_CONTEXTIDR=y
CONFIG_IKCONFIG_PROC=y
CONFIG_CORESIGHT_CPU_DEBUG=y
CONFIG_KALLSYMS_ALL=y
CONFIG_DEBUG_KERNEL=y
CONFIG_PACKAGE_kmod-helloworld=y
EOF
make defconfig

echo 'CONFIG_DEBUG_INFO=y' >> .config
echo 'CONFIG_EXPERT=y' >> .config
echo 'CONFIG_KALLSYMS=y' >> .config
echo 'CONFIG_SYSFS=y' >> .config
echo 'CONFIG_PID_IN_CONTEXTIDR=y' >> .config
echo 'CONFIG_IKCONFIG_PROC=y' >> .config
echo 'CONFIG_CORESIGHT_CPU_DEBUG=y' >> .config
echo 'CONFIG_KALLSYMS_ALL=y' >> .config
echo 'CONFIG_DEBUG_KERNEL=y' >> .config
echo 'CONFIG_PACKAGE_kmod-helloworld=y' >> .config
echo 'CONFIG_CC_OPTIMIZE_FOR_PERFORMANCE=n' >> .config

sed -i 's|CONFIG_KERNEL_DEBUG_INFO_REDUCED=y|CONFIG_KERNEL_DEBUG_INFO_REDUCED=n|g' .config

# add hello world module
cp -rf $TOPDIR/helloworld $HELLO_WORLD_DIR
# Path config.txt
cp -f $TOPDIR/patches/config.txt $TOPDIR/openwrt/target/linux/bcm27xx/image/config.txt
# FIrst build
make -j"$MAKE_JOBS" V=cs > first_build.log 2>&1
# Modify kernel config on the build_dir
sed -i 's|# CONFIG_PID_IN_CONTEXTIDR is not set|CONFIG_PID_IN_CONTEXTIDR=y|g' \
            $OPENWRT_DIR/build_dir/target-aarch64_cortex-a72_musl/linux-bcm27xx_bcm2711/linux-5.15.132/.config
sed -i 's|# CONFIG_KALLSYMS_ALL is not set|CONFIG_KALLSYMS_ALL=y|g' \
            $OPENWRT_DIR/build_dir/target-aarch64_cortex-a72_musl/linux-bcm27xx_bcm2711/linux-5.15.132/.config
echo 'CONFIG_IKCONFIG_PROC=y' >> $OPENWRT_DIR/build_dir/target-aarch64_cortex-a72_musl/linux-bcm27xx_bcm2711/linux-5.15.132/.config
echo 'CONFIG_CORESIGHT_CPU_DEBUG=y' >> $OPENWRT_DIR/build_dir/target-aarch64_cortex-a72_musl/linux-bcm27xx_bcm2711/linux-5.15.132/.config

sed -i 's|MAKEFLAGS += -rR|MAKEFLAGS += -g -rR|g' \
            $OPENWRT_DIR/build_dir/target-aarch64_cortex-a72_musl/linux-bcm27xx_bcm2711/linux-5.15.132/Makefile
if grep -q "KBUILD_HOSTCFLAGS   := " $OPENWRT_DIR/build_dir/target-aarch64_cortex-a72_musl/linux-bcm27xx_bcm2711/linux-5.15.132/Makefile; then
    sed -i 's|KBUILD_HOSTCFLAGS   := |KBUILD_HOSTCFLAGS   := -g -O0 |g' \
            $OPENWRT_DIR/build_dir/target-aarch64_cortex-a72_musl/linux-bcm27xx_bcm2711/linux-5.15.132/Makefile
fi
# And recompile
make -j"$MAKE_JOBS"
########################################################################################################################
# Check build result
########################################################################################################################
if [ $? -eq 0 ]; then
    echo_green "Build process is successful."
    cp -f $OPENWRT_DIR/bin/targets/$TARGET/openwrt-bcm27xx-bcm2711-rpi-4-squashfs-factory.img.gz $TOPDIR
    cd $TOPDIR
    gunzip openwrt-bcm27xx-bcm2711-rpi-4-squashfs-factory.img.gz
    echo_green "Final image is located at: $TOPDIR/openwrt-bcm27xx-bcm2711-rpi-4-squashfs-factory.img"
else
    echo_red "Build failed."
    exit 1
fi