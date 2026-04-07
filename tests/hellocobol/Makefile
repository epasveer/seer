.PHONY: all
all: hellocobol hellocobol_variables hellocobol_if

hellocobol: hellocobol.cob
	cobc -x -g -o hellocobol hellocobol.cob

hellocobol_variables: hellocobol_variables.cob
	cobc -x -g -o hellocobol_variables hellocobol_variables.cob

hellocobol_if: hellocobol_if.cob
	cobc -x -g -o hellocobol_if hellocobol_if.cob

.PHONY: clean
clean:
	rm -f hellocobol hellocobol.o *.i

