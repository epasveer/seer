.PHONY: all
all: hellogopyramid

hellogopyramid: hellogopyramid.go
	go build -gcflags=all="-N -l" hellogopyramid.go

.PHONY: clean
clean:
	rm -f hellogopyramid

