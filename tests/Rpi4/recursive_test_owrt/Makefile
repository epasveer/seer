include $(TOPDIR)/rules.mk

PKG_NAME:=recursive-test
PKG_VERSION:=1.0
PKG_RELEASE:=1

PKG_BUILD_DIR:=$(BUILD_DIR)/$(PKG_NAME)-$(PKG_VERSION)

include $(INCLUDE_DIR)/package.mk

# Keep the installed binary unstripped so it can be debugged on-target and so a
# host copy carries full symbols for Seer/GDB.
RSTRIP:=:

define Package/recursive-test
  SECTION:=utils
  CATEGORY:=Utilities
  TITLE:=math_toolkit - recursion/struct test program for gdbserver + Seer
  URL:=https://github.com/epasveer/seer
endef

define Package/recursive-test/description
  A small C++ program (math_toolkit) with recursion, nested structs and a
  long-running loop. Statically linked and non-PIE, built -g -O0, so remote
  debugging on a Raspberry Pi 4 via gdbserver + Seer needs no shared-library
  relocation and works identically in launch and --attach modes.
endef

define Build/Prepare
	mkdir -p $(PKG_BUILD_DIR)
	$(CP) ./src/*.cpp $(PKG_BUILD_DIR)/
	$(CP) ./include $(PKG_BUILD_DIR)/
endef

define Build/Compile
	$(TARGET_CXX) \
		-g -O0 -Wall -Wextra \
		-static -no-pie -fno-pie \
		-I$(PKG_BUILD_DIR)/include \
		-o $(PKG_BUILD_DIR)/math_toolkit \
		$(PKG_BUILD_DIR)/main.cpp \
		$(PKG_BUILD_DIR)/arithmetic.cpp \
		$(PKG_BUILD_DIR)/advanced.cpp \
		-pthread \
		-Wl,--start-group -lstdc++ -lgcc -lgcc_eh -Wl,--end-group
endef

define Package/recursive-test/install
	$(INSTALL_DIR) $(1)/usr/bin
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/math_toolkit $(1)/usr/bin/math_toolkit
endef

$(eval $(call BuildPackage,recursive-test))
