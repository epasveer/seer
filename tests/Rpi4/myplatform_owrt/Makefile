include $(TOPDIR)/rules.mk

PKG_NAME:=myplatform
PKG_VERSION:=1.0
PKG_RELEASE:=1

PKG_BUILD_DIR:=$(BUILD_DIR)/kernel/$(PKG_NAME)-$(PKG_VERSION)

include $(INCLUDE_DIR)/kernel.mk
include $(INCLUDE_DIR)/package.mk

define KernelPackage/myplatform
  SUBMENU:=Other modules
  TITLE:=Simple platform driver with DT compatible and sysfs
  FILES:=$(PKG_BUILD_DIR)/myplatform.ko
  AUTOLOAD:=$(call AutoLoad,50,myplatform)
endef

define KernelPackage/myplatform/description
  Simple platform driver with DT compatible and sysfs for Raspberry Pi 4
endef

define Build/Prepare
	$(call Build/Prepare/Default)
	mkdir -p $(PKG_BUILD_DIR)
	$(CP) ./src/* $(PKG_BUILD_DIR)/
endef

define Build/Compile
	$(MAKE) -C "$(LINUX_DIR)" \
		$(KERNEL_MAKE_FLAGS) \
		M="$(PKG_BUILD_DIR)" \
		modules
endef

$(eval $(call KernelPackage,myplatform))
