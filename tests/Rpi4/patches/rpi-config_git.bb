DESCRIPTION = "Commented config.txt file for the Raspberry Pi. \
               The Raspberry Pi config.txt file is read by the GPU before \
               the ARM core is initialised. It can be used to set various \
               system configuration parameters."
LICENSE = "MIT"
LIC_FILES_CHKSUM = "file://${COMMON_LICENSE_DIR}/MIT;md5=0835ade698e0bcf8506ecda2f7b4f302"

COMPATIBLE_MACHINE = "^rpi$"

SRCREV = "648ffc470824c43eb0d16c485f4c24816b32cd6f"
SRC_URI = "git://github.com/Evilpaul/RPi-config.git;protocol=https;branch=master \
          "

S = "${WORKDIR}/git"

PR = "r5"

INHIBIT_DEFAULT_DEPS = "1"

PITFT="${@bb.utils.contains("MACHINE_FEATURES", "pitft", "1", "0", d)}"
PITFT22="${@bb.utils.contains("MACHINE_FEATURES", "pitft22", "1", "0", d)}"
PITFT28r="${@bb.utils.contains("MACHINE_FEATURES", "pitft28r", "1", "0", d)}"
PITFT28c="${@bb.utils.contains("MACHINE_FEATURES", "pitft28c", "1", "0", d)}"
PITFT35r="${@bb.utils.contains("MACHINE_FEATURES", "pitft35r", "1", "0", d)}"

VC4GRAPHICS="${@bb.utils.contains("MACHINE_FEATURES", "vc4graphics", "1", "0", d)}"
VC4DTBO ?= "vc4-kms-v3d"
GPIO_IR ?= "18"
GPIO_IR_TX ?= "17"

CAN_OSCILLATOR ?= "16000000"
CAN0_INTERRUPT_PIN ?= "25"
CAN1_INTERRUPT_PIN ?= "24"

ENABLE_UART ??= ""

WM8960="${@bb.utils.contains("MACHINE_FEATURES", "wm8960", "1", "0", d)}"

GPIO_SHUTDOWN_PIN ??= ""

inherit deploy nopackages

do_deploy() {
    install -d ${DEPLOYDIR}/${BOOTFILES_DIR_NAME}
    CONFIG=${DEPLOYDIR}/${BOOTFILES_DIR_NAME}/config.txt

    cp ${S}/config.txt $CONFIG

    if [ -n "${KEY_DECODE_MPG2}" ]; then
        sed -i '/#decode_MPG2=/ c\decode_MPG2=${KEY_DECODE_MPG2}' $CONFIG
    fi
    if [ -n "${KEY_DECODE_WVC1}" ]; then
        sed -i '/#decode_WVC1=/ c\decode_WVC1=${KEY_DECODE_WVC1}' $CONFIG
    fi
    if [ -n "${DISABLE_OVERSCAN}" ]; then
        sed -i '/#disable_overscan=/ c\disable_overscan=${DISABLE_OVERSCAN}' $CONFIG
    fi
    if [ "${DISABLE_SPLASH}" = "1" ]; then
        sed -i '/#disable_splash=/ c\disable_splash=${DISABLE_SPLASH}' $CONFIG
    fi

    # Set overclocking options
    if [ -n "${ARM_FREQ}" ]; then
        sed -i '/#arm_freq=/ c\arm_freq=${ARM_FREQ}' $CONFIG
    fi
    if [ -n "${GPU_FREQ}" ]; then
        sed -i '/#gpu_freq=/ c\gpu_freq=${GPU_FREQ}' $CONFIG
    fi
    if [ -n "${CORE_FREQ}" ]; then
        sed -i '/#core_freq=/ c\core_freq=${CORE_FREQ}' $CONFIG
    fi
    if [ -n "${SDRAM_FREQ}" ]; then
        sed -i '/#sdram_freq=/ c\sdram_freq=${SDRAM_FREQ}' $CONFIG
    fi
    if [ -n "${OVER_VOLTAGE}" ]; then
        sed -i '/#over_voltage=/ c\over_voltage=${OVER_VOLTAGE}' $CONFIG
    fi

    # GPU memory
    if [ -n "${GPU_MEM}" ]; then
        sed -i '/#gpu_mem=/ c\gpu_mem=${GPU_MEM}' $CONFIG
    fi
    if [ -n "${GPU_MEM_256}" ]; then
        sed -i '/#gpu_mem_256=/ c\gpu_mem_256=${GPU_MEM_256}' $CONFIG
    fi
    if [ -n "${GPU_MEM_512}" ]; then
        sed -i '/#gpu_mem_512=/ c\gpu_mem_512=${GPU_MEM_512}' $CONFIG
    fi
    if [ -n "${GPU_MEM_1024}" ]; then
        sed -i '/#gpu_mem_1024=/ c\gpu_mem_1024=${GPU_MEM_1024}' $CONFIG
    fi

    # Set boot delay
    if [ -n "${BOOT_DELAY}" ]; then
        sed -i '/#boot_delay=/ c\boot_delay=${BOOT_DELAY}' $CONFIG
    fi
    if [ -n "${BOOT_DELAY_MS}" ]; then
        sed -i '/#boot_delay_ms=/ c\boot_delay_ms=${BOOT_DELAY_MS}' $CONFIG
    fi

    # Set HDMI and composite video options
    if [ -n "${HDMI_FORCE_HOTPLUG}" ]; then
        sed -i '/#hdmi_force_hotplug=/ c\hdmi_force_hotplug=${HDMI_FORCE_HOTPLUG}' $CONFIG
    fi
    if [ -n "${HDMI_DRIVE}" ]; then
        sed -i '/#hdmi_drive=/ c\hdmi_drive=${HDMI_DRIVE}' $CONFIG
    fi
    if [ -n "${HDMI_GROUP}" ]; then
        sed -i '/#hdmi_group=/ c\hdmi_group=${HDMI_GROUP}' $CONFIG
    fi
    if [ -n "${HDMI_MODE}" ]; then
        sed -i '/#hdmi_mode=/ c\hdmi_mode=${HDMI_MODE}' $CONFIG
    fi
    if [ -n "${HDMI_CVT}" ]; then
        echo 'hdmi_cvt=${HDMI_CVT}' >> $CONFIG
    fi
    if [ -n "${CONFIG_HDMI_BOOST}" ]; then
        sed -i '/#config_hdmi_boost=/ c\config_hdmi_boost=${CONFIG_HDMI_BOOST}' $CONFIG
    fi
    if [ -n "${SDTV_MODE}" ]; then
        sed -i '/#sdtv_mode=/ c\sdtv_mode=${SDTV_MODE}' $CONFIG
    fi
    if [ -n "${SDTV_ASPECT}" ]; then
        sed -i '/#sdtv_aspect=/ c\sdtv_aspect=${SDTV_ASPECT}' $CONFIG
    fi
    if [ -n "${DISPLAY_ROTATE}" ]; then
        sed -i '/#display_rotate=/ c\display_rotate=${DISPLAY_ROTATE}' $CONFIG
    fi

    # Video camera support
    if [ "${VIDEO_CAMERA}" = "1" ]; then
        #   It has been observed that Raspberry Pi 4B 4GB may fail to enable the
        # camera if "start_x=1" is at the end of the file. Therefore,
        # "start_x=1" has been set to replace the original occurrence in
        # config.txt, which is at the middle of the file.
        #   The exact underlying cause is unknown. There are similar issues
        # reported in the raspberrypi/firware repo and the conclusion reached
        # was that there could be a file size limitation affecting certain
        # variables. It was commented that this limitation could be 4k but
        # not proved.
        sed -i '/#start_x=/ c\start_x=1' $CONFIG
    fi

    # Offline compositing support
    if [ "${DISPMANX_OFFLINE}" = "1" ]; then
        echo "# Enable offline compositing" >>$CONFIG
        echo "dispmanx_offline=1" >>$CONFIG
    fi

    # SPI bus support
    if [ "${ENABLE_SPI_BUS}" = "1" ] || [ "${PITFT}" = "1" ]; then
        echo "# Enable SPI bus" >>$CONFIG
        echo "dtparam=spi=on" >>$CONFIG
    fi

    # I2C support
    if [ "${ENABLE_I2C}" = "1" ] || [ "${PITFT}" = "1" ]; then
        echo "# Enable I2C" >>$CONFIG
        echo "dtparam=i2c1=on" >>$CONFIG
        echo "dtparam=i2c_arm=on" >>$CONFIG
    fi

    # PiTFT22 display support
    if [ "${PITFT22}" = "1" ]; then
        echo "# Enable PITFT22 display" >>$CONFIG
        echo "dtoverlay=pitft22,rotate=270,speed=32000000,txbuflen=32768" >>$CONFIG
    fi
    if [ "${PITFT28r}" = "1" ]; then
        echo "# Enable PITFT28r display" >>$CONFIG
        echo "dtoverlay=pitft28-resistive,rotate=90,speed=32000000,txbuflen=32768" >>$CONFIG
    fi
    if [ "${PITFT28c}" = "1" ]; then
        echo "# Enable PITFT28c display" >>$CONFIG
        echo "dtoverlay=pitft28-capacitive,rotate=90,speed=32000000,txbuflen=32768" >>$CONFIG
        echo "dtoverlay=pitft28-capacitive,touch-swapxy,touch-invx" >>$CONFIG
    fi
    if [ "${PITFT35r}" = "1" ]; then
        echo "# Enable PITFT35r display" >>$CONFIG
        echo "dtoverlay=pitft35-resistive,rotate=90,speed=42000000,fps=20" >>$CONFIG
    fi

    # UART support
    if [ "${ENABLE_UART}" = "1" ] || [ "${ENABLE_UART}" = "0" ] ; then
        echo "# Enable UART" >>$CONFIG
        echo "enable_uart=${ENABLE_UART}" >>$CONFIG
    elif [ -n "${ENABLE_UART}" ]; then
        bbfatal "Invalid value for ENABLE_UART [${ENABLE_UART}]. The value for ENABLE_UART can be 0 or 1."
    fi

	# Enable JTAG
    if [ "${ENABLE_JTAG}" = "1" ] || [ "${ENABLE_JTAG}" = "0" ] ; then
        echo "# Disable pull downs" >>$CONFIG
        echo "gpio=22-27=np" >>$CONFIG
		echo "enable_jtag_gpio=1" >>$CONFIG
    elif [ -n "${ENABLE_JTAG}" ]; then
        bbfatal "Invalid value for ENABLE_JTAG [${ENABLE_JTAG}]. The value for ENABLE_JTAG can be 0 or 1."
    fi

    # Infrared support
    if [ "${ENABLE_IR}" = "1" ]; then
        echo "# Enable infrared" >>$CONFIG
        echo "dtoverlay=gpio-ir,gpio_pin=${GPIO_IR}" >>$CONFIG
        echo "dtoverlay=gpio-ir-tx,gpio_pin=${GPIO_IR_TX}" >>$CONFIG
    fi

    # VC4 Graphics support
    if [ "${VC4GRAPHICS}" = "1" ]; then
        echo "# Enable VC4 Graphics" >> $CONFIG
        echo "dtoverlay=${VC4DTBO}" >> $CONFIG
    fi

    # Choose Camera Sensor to be used, default imx219 sensor
    if [ "${RASPBERRYPI_CAMERA_V2}" = "1" ]; then
        echo "# Enable Sony RaspberryPi Camera(imx219)" >> $CONFIG
        echo "dtoverlay=imx219" >> $CONFIG
    fi

    # Choose Camera Sensor to be used, default imx477 sensor
    #if [ "${RASPBERRYPI_HD_CAMERA}" = "1" ]; then
    #    echo "# Enable Sony RaspberryPi Camera(imx477)" >> $CONFIG
    #    echo "dtoverlay=imx477" >> $CONFIG
    #fi

    # Waveshare "C" 1024x600 7" Rev2.1 IPS capacitive touch (http://www.waveshare.com/7inch-HDMI-LCD-C.htm)
    if [ "${WAVESHARE_1024X600_C_2_1}" = "1" ]; then
        echo "# Waveshare \"C\" 1024x600 7\" Rev2.1 IPS capacitive touch screen" >> $CONFIG
        echo "max_usb_current=1" >> $CONFIG
        echo "hdmi_group=2" >> $CONFIG
        echo "hdmi_mode=87" >> $CONFIG
        echo "hdmi_cvt 1024 600 60 6 0 0 0" >> $CONFIG
        echo "hdmi_drive=1" >> $CONFIG
    fi

    # DWC2 USB peripheral support
    if ([ "${ENABLE_DWC2_PERIPHERAL}" = "1" ] && [ "${ENABLE_DWC2_OTG}" != "1" ]); then
        echo "# Enable USB peripheral mode" >> $CONFIG
        echo "dtoverlay=dwc2,dr_mode=peripheral" >> $CONFIG
    fi

    # DWC2 USB host mode support
    if [ "${ENABLE_DWC2_HOST}" = "1" ]; then
        echo "# Enable USB host mode" >> $CONFIG
        echo "dtoverlay=dwc2,dr_mode=host" >> $CONFIG
    fi
    
    # DWC2 USB OTG support
    if ([ "${ENABLE_DWC2_OTG}" = "1" ] && [ "${ENABLE_DWC2_PERIPHERAL}" != "1" ]); then
        echo "# Enable USB OTG mode" >> $CONFIG
        echo "dtoverlay=dwc2,dr_mode=otg" >> $CONFIG
    fi

    # AT86RF23X support
    if [ "${ENABLE_AT86RF}" = "1" ]; then
        echo "# Enable AT86RF23X" >>$CONFIG
        echo "dtoverlay=at86rf233,speed=3000000" >>$CONFIG
    fi

    # ENABLE DUAL CAN
    if [ "${ENABLE_DUAL_CAN}" = "1" ]; then
        echo "# Enable DUAL CAN" >>$CONFIG
        echo "dtoverlay=mcp2515-can0,oscillator=${CAN_OSCILLATOR},interrupt=${CAN0_INTERRUPT_PIN}" >>$CONFIG
        echo "dtoverlay=mcp2515-can1,oscillator=${CAN_OSCILLATOR},interrupt=${CAN1_INTERRUPT_PIN}" >>$CONFIG
    # ENABLE CAN
    elif [ "${ENABLE_CAN}" = "1" ]; then
        echo "# Enable CAN" >>$CONFIG
        echo "dtoverlay=mcp2515-can0,oscillator=${CAN_OSCILLATOR},interrupt=${CAN0_INTERRUPT_PIN}" >>$CONFIG
    fi


    if [ "${ENABLE_GPIO_SHUTDOWN}" = "1" ]; then
        if ([ "${ENABLE_I2C}" = "1" ] || [ "${PITFT}" = "1" ]) && [ -z "${GPIO_SHUTDOWN_PIN}" ]; then
            # By default GPIO shutdown uses the same pin as the (master) I2C SCL.
            # If I2C is configured and an alternative pin is not configured for
            # gpio-shutdown, there is a configuration conflict.
            bbfatal "I2C and gpio-shutdown are both enabled and using the same pins!"
        fi
        echo "# Enable gpio-shutdown" >> $CONFIG
        if [ -z "${GPIO_SHUTDOWN_PIN}" ]; then
            echo "dtoverlay=gpio-shutdown" >> $CONFIG
        else
            echo "dtoverlay=gpio-shutdown,gpio_pin=${GPIO_SHUTDOWN_PIN}" >> $CONFIG
        fi
    fi

    # Append extra config if the user has provided any
    printf "${RPI_EXTRA_CONFIG}\n" >> $CONFIG

    # Handle setup with armstub file
    if [ "${@bb.utils.contains("MACHINE_FEATURES", "armstub", "1", "0", d)}" = "1" ]; then
        echo "\n# ARM stub configuration" >> $CONFIG
        echo "armstub=${ARMSTUB}" >> $CONFIG
        case "${ARMSTUB}" in
            *-gic.bin)
                echo  "enable_gic=1" >> $CONFIG
                ;;
        esac
    fi

    # WM8960 support
    if [ "${WM8960}" = "1" ]; then
        echo "# Enable WM8960" >> $CONFIG
        echo "dtoverlay=wm8960-soundcard" >> $CONFIG
    fi

    # W1-GPIO - One-Wire Interface
    if [ "${ENABLE_W1}" = "1" ]; then
        echo "# Enable One-Wire Interface" >> $CONFIG
        echo "dtoverlay=w1-gpio" >> $CONFIG
    fi

    # Reduce config.txt file size to avoid corruption and
    # to boot successfully Raspberry Pi 5. The issue has
    # been reported to related projects:
    # https://github.com/raspberrypi/firmware/issues/1848
    # https://github.com/Evilpaul/RPi-config/issues/9
    sed -i '/^##/d' $CONFIG
}

do_deploy:append:raspberrypi3-64() {
    echo "# have a properly sized image" >> $CONFIG
    echo "disable_overscan=1" >> $CONFIG

    echo "# Enable audio (loads snd_bcm2835)" >> $CONFIG
    echo "dtparam=audio=on" >> $CONFIG
}

do_deploy:append() {
    if grep -q -E '^.{80}.$' ${DEPLOYDIR}/${BOOTFILES_DIR_NAME}/config.txt; then
        bbwarn "config.txt contains lines longer than 80 characters, this is not supported"
    fi
}

addtask deploy before do_build after do_install
do_deploy[dirs] += "${DEPLOYDIR}/${BOOTFILES_DIR_NAME}"

PACKAGE_ARCH = "${MACHINE_ARCH}"
