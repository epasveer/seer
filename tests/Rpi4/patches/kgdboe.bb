SUMMARY = "KGDB over Ethernet kernel module"
LICENSE = "GPL-2.0-only"
LIC_FILES_CHKSUM = "file://COPYING;md5=b234ee4d69f5fce4486a80fdaf4a4263"

SRC_URI = "git://github.com/sysprogs/kgdboe.git;protocol=https;branch=master"
SRCREV = "${AUTOREV}"

S = "${WORKDIR}/git"

inherit module

# The module needs kallsyms_lookup_name on some kernels
EXTRA_OEMAKE += "KCFLAGS='-DCONFIG_KGDBOE_USE_KALLSYMS=1'"

do_install:append() {
    # Optional: install a helper script
    install -d ${D}${bindir}
    cat > ${D}${bindir}/load-kgdboe << EOF
#!/bin/sh
echo "Loading KGDBoE..."
insmod /lib/modules/\$(uname -r)/extra/kgdboe.ko device_name=eth0 udp_port=31337
EOF
    chmod +x ${D}${bindir}/load-kgdboe
}

FILES:${PN} += "${bindir}/load-kgdboe"