.PHONY: all
all: check_money

check_money: check_money.c money.c
	g++ -I/usr/local/include -L/usr/local/lib64 -lcheck -g -o check_money check_money.c money.c

.PHONY: clean
clean:
	rm -f check_money check_money.o money.o

