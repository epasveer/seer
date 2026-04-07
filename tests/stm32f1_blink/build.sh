set -x
TOP_DIR=${PWD}
git clone https://github.com/STMicroelectronics/STM32CubeF1.git vendor
cd vendor
git submodule update --init --recursive
cd ..
# install package
sudo apt install gcc-arm-none-eabi
# make
make
# make flash, assume that openocd is installed by running install_openocd.sh
make flash JLINK_CFG=${PWD}/../openocd/tcl/interface/jlink.cfg

