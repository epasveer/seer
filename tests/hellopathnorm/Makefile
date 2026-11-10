# Regression test for the path normalization layers.
# Links the real SeerUtl/GdbMonitor sources (moc included), so the
# actual production code is exercised.
.PHONY: all
all: hellopathnorm

MOC     = $(shell command -v moc 2>/dev/null || echo /usr/lib/qt6/libexec/moc)
QT_PKGS = Qt6Core Qt6Widgets Qt6Charts
SRC     = hellopathnorm.cpp ../../src/SeerUtl.cpp ../../src/GdbMonitor.cpp moc_GdbMonitor.cpp
HDR     = -I../../src

moc_GdbMonitor.cpp: ../../src/GdbMonitor.h
	$(MOC) $< -o $@

hellopathnorm: $(SRC) ../../src/GdbMonitor.h
	g++ -fPIC -o $@ $(SRC) $(HDR) $(shell pkg-config --cflags --libs $(QT_PKGS))

.PHONY: clean
clean:
	rm -f hellopathnorm moc_GdbMonitor.cpp
