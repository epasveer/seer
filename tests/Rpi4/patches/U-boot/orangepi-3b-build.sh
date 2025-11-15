#!/bin/bash

# Delete the current configuration, and all generated files
make mrproper

# Copy the proprietary blobs to a temporary directory
tmpdir=$(mktemp --directory)
cp --recursive rkbin "$tmpdir"

# Rockchip's proprietary TPL to initialize the DRAM
export ROCKCHIP_TPL="$tmpdir"/rkbin/bin/rk35/rk3566_ddr_1056MHz_v1.21.bin
# Rockchip's proprietary TF-A to initialize the secure environment
export BL31="$tmpdir"/rkbin/bin/rk35/rk3568_bl31_v1.44.elf

# Apply the default configuration for the Orange Pi 3B
make CROSS_COMPILE=aarch64-linux-gnu- orangepi-3b-rk3566_defconfig
# Build the kernel image and device tree
make CROSS_COMPILE=aarch64-linux-gnu- --jobs all

# Clean up the temporary directory
rm --recursive --force "$tmpdir"