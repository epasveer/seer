Notes for building Seer with cmake.

    % cd seer/src/build
    % cmake -DCMAKE_BUILD_TYPE=Debug ..     # Debug release -g.
    % cmake -DCMAKE_BUILD_TYPE=Release ..   # Optimized release -O.
    % cmake -DCMAKE_CXX_FLAGS=-Wall ..      # With all compile warnings turned on.


    % make clean                            # Clean files.
    % make seergdb                          # Build Seer.
    % sudo make install                     # Install it.

    % cmake --build . --config Release
    % sudo cmake --install .

MacOS may need help finding the cmake config file for Qt5.

    % cmake -DCMAKE_PREFIX_PATH=/usr/local/opt/qt5/ -DCMAKE_BUILD_TYPE=Release ..

