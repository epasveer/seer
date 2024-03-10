prefix = /usr
bindir = $(prefix)/bin
sysconfdir = /etc
unitdir = $(sysconfdir)/systemd/system

OBJS = pipowerd.o

UNITS = \
	pipower-boot.service \
	pipowerd.service

%.pre: %.c
	$(CC) $(CFLAGS) -E -o $@ $<

all: pipowerd

pipowerd: $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $< $(LIBS)

clean:
	rm -f pipowerd $(OBJS)

install: install-bin install-units

install-bin: pipowerd
	install -m 755 -d $(DESTDIR)$(bindir)
	install -m 755 pipowerd $(DESTDIR)$(bindir)

install-units:
	install -m 755 -d $(DESTDIR)$(unitdir)
	install -m 644 $(UNITS) $(DESTDIR)$(unitdir)

activate:
	systemctl daemon-reload
	systemctl enable --now $(UNITS)
