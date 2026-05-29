SUMMARY = "Simple platform driver with device tree compatible and sysfs for Raspberry Pi 4"
LICENSE = "GPL-2.0-only"
LIC_FILES_CHKSUM = "file://${COMMON_LICENSE_DIR}/GPL-2.0-only;md5=801f80980d171dd6425610833a22dbe6"

inherit module

SRC_URI = "file://myplatform.c \
           file://Makefile"

S = "${WORKDIR}"

INSANE_SKIP += "debug-files"
