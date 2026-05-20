include $(TOPDIR)/rules.mk

PKG_NAME:=helloworld
PKG_VERSION:=1.0
PKG_RELEASE:=1

PKG_BUILD_DIR:=$(BUILD_DIR)/kernel/$(PKG_NAME)-$(PKG_VERSION)

include $(INCLUDE_DIR)/kernel.mk
include $(INCLUDE_DIR)/package.mk

define KernelPackage/helloworld
  SUBMENU:=Other modules
  TITLE:=Advanced Hello World kernel module with sysfs
  FILES:=$(PKG_BUILD_DIR)/helloworld.ko
  AUTOLOAD:=$(call AutoLoad,50,helloworld)
endef

define KernelPackage/helloworld/description
  An advanced Hello World kernel module with sysfs and Fibonacci calculation for Raspberry Pi 4
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

$(eval $(call KernelPackage,helloworld))
