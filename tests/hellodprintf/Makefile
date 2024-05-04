.PHONY: all
all: hellodprintf

hellodprintf.o: hellodprintf.cpp
	g++ -c -g hellodprintf.cpp

hellodprintf_fortran.o: hellodprintf_fortran.f
	gfortran -c -g hellodprintf_fortran.f

hellodprintf: hellodprintf.o hellodprintf_fortran.o
	g++ -g -o hellodprintf hellodprintf.o hellodprintf_fortran.o -lgfortran

.PHONY: clean
clean:
	rm -f hellodprintf hellodprintf.o hellodprintf_fortran.o

