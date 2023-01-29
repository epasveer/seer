.PHONY: all
all: hellogo

hellogo: hellogo.go
	go build -gcflags=all="-N -l" hellogo.go

.PHONY: clean
clean:
	rm -f hellogo

