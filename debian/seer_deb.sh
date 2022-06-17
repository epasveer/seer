#!/bin/bash

# Build a deb package for the latest version of the Seer debugger.
# Run as: seer_deb /path/to/build_directory

if [ $1"MISSING" == "MISSING" ] ; then
	echo "usage: seer_deb /path/to/build_directory"
	exit
fi

BUILD_DIR=$(readlink -f "$1")
mkdir -p "$BUILD_DIR"

# install prerequisites
prereqs=""
for p in "curl" "build-essential" "cmake" "qtbase5-dev" "libqt5charts5-dev" ; do
	if dpkg-query -W -f='${Status}' $p | grep -q installed ; then
		echo "$p already installed"
	else
		prereqs="$prereqs $p"
	fi
done
if [ $prereqs"NONE" != "NONE" ] ; then
	echo sudo apt-get install "$prereqs"
fi

# download latest version from github
URL="https://github.com/epasveer/seer"
latest=$(curl -sSL "$URL" | grep -oEi "/releases/tag/v([0-9.]+)" | awk -F '/v' '{print $2}')
latest_file="$BUILD_DIR/seer-$latest.tar.gz"
if [ ! -e "$latest_file" ] ; then
	latest_url="$URL/archive/refs/tags/v$latest.tar.gz"
	curl -SL "$latest_url" -o "$latest_file"
fi

# extract downloaded archive
TOP_DIR="$BUILD_DIR/seer-$latest"
if [ ! -e "$TOP_DIR" ]; then
	tar -xv -C "$BUILD_DIR" --file $latest_file
fi

# build
cd "$TOP_DIR"/src/build
cmake ..
make seer

# install to package directory

DEB_DIR="$BUILD_DIR"/seer_"$latest"_amd64
rm -rf "$DEB_DIR"

mkdir -p "$DEB_DIR"/usr/bin
install -s -m 755 "$TOP_DIR"/src/build/seer "$DEB_DIR"/usr/bin

mkdir -p "$DEB_DIR"/usr/share/doc/seer
install -m 644 "$TOP_DIR"/README.md "$TOP_DIR"/LICENSE "$DEB_DIR"/usr/share/doc/seer

# add desktop file for menu
mkdir -p "$DEB_DIR"/usr/share/applications
cat << EOF > "$DEB_DIR"/usr/share/applications/seer.desktop
[Desktop Entry]
Name=Seer
GenericName=Debugger
Comment=GUI frontend to gdb.
Exec=seer
Icon=seer
Terminal=false
Type=Application
Categories=Development;
EOF

# add application icons
for size in 32 64 128 256 512; do
	mkdir -p "$DEB_DIR"/usr/share/icons/hicolor/"$size"x"$size"/apps
	install -m 644 "$TOP_DIR"/src/resources/seer_"$size"x"$size".png "$DEB_DIR"/usr/share/icons/hicolor/"$size"x"$size"/apps/seer.png
done

# figure out library dependencies
deps_file="/dev/shm/seer-deps.txt"
rm -f "$deps_file"
touch "$deps_file"
ldd "$TOP_DIR"/src/build/seer | while read -r line; do
	if [[ $line == *" => "* ]]; then
		lib=$(echo $line | cut -d' ' -f 1)
		package_info=$(dpkg-query -S $lib)
		package=$(echo $package_info | cut -d':' -f 1)
		echo $package >> "$deps_file"
	fi
done
deps=$(sort -u "$deps_file" | paste -sd ',')
rm -f "$deps_file"

# generate debian control file
mkdir -p "$DEB_DIR"/DEBIAN
cat << EOF > "$DEB_DIR"/DEBIAN/control
Package: seer
Version: $latest
Section: devel
Priority: optional
Maintainer: H. Turgut Uyar <uyar@tekir.org>
Homepage: https://github.com/epasveer/seer
Architecture: amd64
Depends: gdb,$deps
Description: A GUI frontend to gdb.
EOF

# generate the deb package
dpkg-deb --build --root-owner-group "$DEB_DIR"
