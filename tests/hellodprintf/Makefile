.PHONY: all
all: hellodprintf hellodprintf_call_channel hellodprintf_call_nochannel hellodprintf_fortran

hellodprintf.o: hellodprintf.cpp
	g++ -c -g hellodprintf.cpp

hellodprintf_fortran.o: hellodprintf_fortran.cpp
	g++ -c -g hellodprintf_fortran.cpp

hellodprintf_call_channel.o: hellodprintf_call_channel.cpp
	g++ -c -g hellodprintf_call_channel.cpp

hellodprintf_call_nochannel.o: hellodprintf_call_nochannel.cpp
	g++ -c -g hellodprintf_call_nochannel.cpp

fortran_function.o: fortran_function.f
	gfortran -c -g fortran_function.f

hellodprintf: hellodprintf.o hellodprintf_fortran.o fortran_function.o
	g++ -g -o hellodprintf hellodprintf.o fortran_function.o -lgfortran

hellodprintf_call_channel: hellodprintf_call_channel.o
	g++ -g -o hellodprintf_call_channel hellodprintf_call_channel.o

hellodprintf_call_nochannel: hellodprintf_call_nochannel.o
	g++ -g -o hellodprintf_call_nochannel hellodprintf_call_nochannel.o

hellodprintf_fortran: hellodprintf_fortran.o fortran_function.o
	g++ -g -o hellodprintf_fortran hellodprintf_fortran.o fortran_function.o -lgfortran

.PHONY: clean
clean:
	rm -f hellodprintf hellodprintf_call_channel hellodprintf_call_nochannel hellodprintf_fortran *.o

