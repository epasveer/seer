.PHONY: all
all: hellogdbserver hellogdbserver_sym

hellogdbserver_sym: hellogdbserver.c
	gcc -g -o hellogdbserver_sym hellogdbserver.c -lm

hellogdbserver: hellogdbserver_sym
	strip -g -o hellogdbserver hellogdbserver_sym

.PHONY: clean
clean:
	rm -f hellogdbserver_sym hellogdbserver.o hellogdbserver

