.POSIX:

JLINK_CFG :=

TARGET := main
BUILD_DIR := build
INC_DIR := include
SRC_DIR := src

MCU_VARIANT := STM32F103x6
CPU := cortex-m3
F_CPU := 72000000L

CUBE_DIR := vendor
VERBOSE := 0

###############################################################################

CROSS_COMPILE := arm-none-eabi
AS := $(CROSS_COMPILE)-as
AR := $(CROSS_COMPILE)-ar
CC := $(CROSS_COMPILE)-gcc
LD := $(CROSS_COMPILE)-ld
OBJCOPY := $(CROSS_COMPILE)-objcopy
OBJDUMP := $(CROSS_COMPILE)-objdump
SIZE := $(CROSS_COMPILE)-size
GDB := $(CROSS_COMPILE)-gdb

###############################################################################

MCU_VARIANT_LC := $(shell echo $(MCU_VARIANT) | tr "[:upper:]" "[:lower:]")
MCU_VARIANT_UC := $(subst x,X,$(MCU_VARIANT))
MCU_FAMILY_LC := $(shell v=$(MCU_VARIANT_LC); echo $${v%????}xx)
MCU_FAMILY_MC := $(shell v=$(MCU_VARIANT_UC); echo $${v%????}xx)
OPENOCD_TARGET := $(shell v=$(MCU_VARIANT_LC); echo "target/$${v%????}x.cfg")

HAL_DIR := $(CUBE_DIR)/Drivers/$(MCU_FAMILY_MC)_HAL_Driver
CMSIS_DIR := $(CUBE_DIR)/Drivers/CMSIS
DEVICE_DIR := $(CMSIS_DIR)/Device/ST/$(MCU_FAMILY_MC)

HAL_MODULES = $(shell grep -o '^#define HAL_.*_MODULE_ENABLED' \
	$(INC_DIR)/$(MCU_FAMILY_LC)_hal_conf.h | \
	cut -d'_' -f2 | tr '[:upper:]' '[:lower:]')
HAL_SRCS := $(MCU_FAMILY_LC)_hal.c \
	$(addprefix $(MCU_FAMILY_LC)_hal_, $(addsuffix .c,$(HAL_MODULES)))

# Prettify output
ifeq ($(VERBOSE), 0)
	Q = @
	P = > /dev/null
endif

###############################################################################

# Source search paths
VPATH := $(SRC_DIR)
VPATH += $(HAL_DIR)/Src
VPATH += $(DEVICE_DIR)/Source
VPATH += $(DEVICE_DIR)/Source/Templates
VPATH += $(DEVICE_DIR)/Source/Templates/gcc

S_SRCS := startup_$(MCU_VARIANT_LC).s
S_OBJS += $(addprefix $(BUILD_DIR)/,$(S_SRCS:.s=.o))

C_SRCS := $(subst $(SRC_DIR)/,,$(wildcard $(SRC_DIR)/*.c))
C_SRCS += $(HAL_SRCS)
C_SRCS += system_$(MCU_FAMILY_LC).c
C_OBJS := $(addprefix $(BUILD_DIR)/,$(C_SRCS:.c=.o))

DEFS := -D$(MCU_VARIANT) -DF_CPU=$(F_CPU)

INCS := -I$(INC_DIR)
INCS += -I$(CMSIS_DIR)/Include
INCS += -I$(DEVICE_DIR)/Include
INCS += -I$(HAL_DIR)/Inc

COMMON_FLAGS := -mthumb -mcpu=$(CPU) -mfloat-abi=soft
LDFILE := $(DEVICE_DIR)/Source/Templates/gcc/linker/$(MCU_VARIANT_UC)_FLASH.ld

ASFLAGS := $(COMMON_FLAGS)
CFLAGS := $(COMMON_FLAGS) -Wall -g -Os
CFLAGS += -ffunction-sections -fdata-sections
CFLAGS += -nostdlib $(INCS) $(DEFS)
LDFLAGS := $(COMMON_FLAGS) -Os -L$(CMSIS_DIR)/Lib
LDFLAGS += --specs=nano.specs --specs=nosys.specs
LDFLAGS += -Wl,--gc-sections,--relax
LDFLAGS += -Wl,-Map=$(BUILD_DIR)/$(TARGET).map,-T$(LDFILE)

###############################################################################

all: $(BUILD_DIR)/$(TARGET).hex

dirs: $(BUILD_DIR)
$(BUILD_DIR):
	@echo "[MKDIR]   $@"
	$Qmkdir -p $@

$(S_OBJS): $(BUILD_DIR)/%.o: %.s | dirs
	@echo "[AS]      $(notdir $<)"
	$Q$(COMPILE.s) -o $@ $<

$(C_OBJS): $(BUILD_DIR)/%.o: %.c | dirs
	@echo "[CC]      $(notdir $<)"
	$Q$(COMPILE.c) -o $@ $<

$(BUILD_DIR)/$(TARGET).elf: $(C_OBJS) $(S_OBJS)
	@echo "[LD]      $@"
	$Q$(LINK.o) $^ $(LDLIBS) -o $@
	@echo "[OBJDUMP] $(BUILD_DIR)/$(TARGET).lst"
	$Q$(OBJDUMP) -St $@ >$(BUILD_DIR)/$(TARGET).lst
	@echo "[SIZE]    $@"
	$Q$(SIZE) $@

$(BUILD_DIR)/$(TARGET).hex: $(BUILD_DIR)/$(TARGET).elf
	@echo "[OBJCOPY] $@"
	$Q$(OBJCOPY) -O ihex -R .eeprom $< $@

flash: $(BUILD_DIR)/$(TARGET).elf
	openocd -f $(JLINK_CFG) -f $(OPENOCD_TARGET) \
		-c "program $< verify reset exit"

clean:
	@echo "[RM]      $(BUILD_DIR)"; $(RM) -r $(BUILD_DIR)

.PHONY: all program debug clean dirs
