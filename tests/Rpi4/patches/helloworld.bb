SUMMARY = "Advanced Hello World kernel module with sysfs and Fibonacci calculation for Raspberry Pi 4"
LICENSE = "GPL-2.0-only"
LIC_FILES_CHKSUM = "file://${COMMON_LICENSE_DIR}/GPL-2.0-only;md5=801f80980d171dd6425610833a22dbe6"

inherit module

SRC_URI = "file://helloworld.c \
           file://Makefile"

S = "${WORKDIR}"

# Module is built with -g; kernel-module-split keeps the .debug dir in the
# non-debug sub-package which triggers this QA check.
INSANE_SKIP += "debug-files"
