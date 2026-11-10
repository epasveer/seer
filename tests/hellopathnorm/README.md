Self-checking regression test for the path normalization layers.

Covers:

  1. Seer::normalizeSourcePath  — gdb command layer (separators only)
  2. Seer::normalizeFileKey     — key layer (separators + case folding)
  3. GdbMonitor                 — record-level 'fullname="..."' value
     normalization, fed real MI wire lines through a fake 'gdb'
     process wired exactly like SeerGdbWidget wires the real one

Expectations are platform-conditional by design: Windows transforms,
everything else is the identity. Exits 0 when all checks pass.

Build and run (Linux / WSL):

    make
    ./hellopathnorm

Build and run (Windows, MSYS2 mingw64 — from the Git Bash, with the
mingw64 root below referred to as <MINGW64>):

    export PATH=/<MINGW64>:$PATH
    <MINGW64>/share/qt6/bin/moc.exe ../../src/GdbMonitor.h -o moc_GdbMonitor.cpp
    g++ -fPIC -o hellopathnorm.exe \
        hellopathnorm.cpp ../../src/SeerUtl.cpp ../../src/GdbMonitor.cpp moc_GdbMonitor.cpp \
        -I../../src -I<MINGW64>/include/qt6 \
        -L<MINGW64>/lib -lQt6Core -lQt6Gui -lQt6Widgets -lQt6Charts
    ./hellopathnorm.exe

(SeerUtl pulls in QtWidgets and QtCharts; GdbMonitor needs its moc output
linked.)
