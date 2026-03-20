set -x
TOP_DIR=${PWD}
git clone https://github.com/STMicroelectronics/STM32CubeF1.git vendor
cd vendor
git submodule update --init --recursive
cd ..
# make
make
