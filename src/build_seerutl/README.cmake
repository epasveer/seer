Notes for building 'seerutl' with cmake. 'seerutl' is a test program
for testing the SeerUtl functions. It is used for my developent. It
is NOT needed for the average person installing Seergdb from source.

    # Note the single '.'. The CMakeLists.txt file is local to this build directory.
    % cd seer/src/build_seerutl
    % cmake -DCMAKE_BUILD_TYPE=Debug .     # Debug release -g.
    % cmake -DCMAKE_BUILD_TYPE=Release .   # Optimized release -O.
    % cmake -DCMAKE_CXX_FLAGS=-Wall .      # With all compile warnings turned on.

    # Specify a compiler version with debug.
    % cmake -DCMAKE_CXX_COMPILER=g++-13 -DCMAKE_BUILD_TYPE=Debug .

The above defaults to Qt6. If you want to use Qt5, add the '-DQTVERSION' flag.

    % cmake -DQTVERSION=QT5 -DCMAKE_BUILD_TYPE=Debug .

Once cmake is run, then just build 'seerutl'.

    % make clean                            # Clean files.
    % make seerutl                          # Build 'seerutl'.
    % sudo make install                     # Install it.

Another way to ask cmake to do the build for you.

    % cmake --build . --config Release
    % sudo cmake --install .

MacOS may need help finding the cmake config file for Qt6.

    % cmake -DCMAKE_PREFIX_PATH=/usr/local/opt/qt6/ -DCMAKE_BUILD_TYPE=Release ..

Sometimes you need to specify a newer version of g++/gcc. Your installation
may have an old version as the default.

    % cmake -DCMAKE_C_COMPILER=gcc-13 -DCMAKE_CXX_COMPILER=g++-13 -DQTVERSION=QT6 -DCMAKE_BUILD_TYPE=Debug ..

