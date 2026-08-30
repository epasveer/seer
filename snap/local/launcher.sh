#!/bin/sh
# Launcher for the classic-confinement seergdb snap.
#
# Classic snaps run unconfined against the host system (needed here since
# Seer/gdb must ptrace arbitrary host processes and read arbitrary source
# trees), but they don't get automatic library injection the way strict
# snaps do. This wrapper points the dynamic linker and Qt at the copies of
# their runtime libraries/plugins bundled inside the snap, so seergdb works
# even on a host that doesn't have the matching Qt6 packages installed.

case "$(uname -m)" in
    x86_64)  ARCH_TRIPLET=x86_64-linux-gnu ;;
    aarch64) ARCH_TRIPLET=aarch64-linux-gnu ;;
    armv7l)  ARCH_TRIPLET=arm-linux-gnueabihf ;;
    i686)    ARCH_TRIPLET=i386-linux-gnu ;;
    *)       ARCH_TRIPLET="$(uname -m)-linux-gnu" ;;
esac

export LD_LIBRARY_PATH="$SNAP/lib/$ARCH_TRIPLET:$SNAP/usr/lib/$ARCH_TRIPLET:$SNAP/usr/lib:${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}"
export QT_PLUGIN_PATH="$SNAP/usr/lib/$ARCH_TRIPLET/qt6/plugins"
export QT_QPA_PLATFORM_PLUGIN_PATH="$QT_PLUGIN_PATH/platforms"

exec "$SNAP/usr/bin/seergdb" "$@"
