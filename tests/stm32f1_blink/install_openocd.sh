set -x
git clone --recurse-submodules https://github.com/openocd-org/openocd.git
cd openocd
git submodule update --init --recursive
sudo apt install -y autoconf libtool libusb-1.0-0-dev libusb-dev libftdi1-dev automake libftfi-dev \
    apt-utils pkg-config libjaylink0
sudo apt install -y gdb-multiarch
sudo apt install -y libusb-1.0-0-dev libjim-dev
sudo apt install -y libjaylink-dev
sudo apt install -y libhidapi-dev
./bootstrap
cd ..
git clone https://github.com/msteveb/jimtcl.git
cd jimtcl
./configure
sudo make install
cd ..
git clone https://github.com/syntacore/libjaylink.git
cd libjaylink
./augogen.sh
./configure
make -j4
sudo make install
cd ..
cd openocd
./configure --enable-ftdi --enable-ft232r --enable-jlink --enable-armjtagew --enable-parport --enable-stlink
make -j4
sudo make install