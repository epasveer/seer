Notes for building Seer with cmake.

    % cd seer/src/build
    % cmake -DCMAKE_BUILD_TYPE=Debug ..     # Debug release -g.
    % cmake -DCMAKE_BUILD_TYPE=Release ..   # Optimized release -O.
    % cmake -DCMAKE_CXX_FLAGS=-Wall ..      # With all compile warnings turned on.

The above defaults to Qt6. If you want to use Qt5, add the '-DQTVERSION' flag.

    % cmake -DQTVERSION=QT5 -DCMAKE_BUILD_TYPE=Debug ..

Once cmake is run, then just build Seer.

    % make clean                            # Clean files.
    % make seergdb                          # Build Seer.
    % sudo make install                     # Install it.

Another way to ask cmake to do the build for you.

    % cmake --build . --config Release
    % sudo cmake --install .

MacOS may need help finding the cmake config file for Qt6.

    % cmake -DCMAKE_PREFIX_PATH=/usr/local/opt/qt6/ -DCMAKE_BUILD_TYPE=Release ..

Checkout the Seer wiki for more build info.

    https://github.com/epasveer/seer/wiki/Building-Seer---Qt5
    https://github.com/epasveer/seer/wiki/Building-Seer---Qt6

