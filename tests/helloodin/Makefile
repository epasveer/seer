.PHONY: all
all: demo helloodin

demo: demo.odin
	odin build demo.odin -file -debug -out:demo

helloodin: helloodin.odin
	odin build helloodin.odin -file -debug -out:helloodin

.PHONY: clean
clean:
	rm -f demo helloodin

