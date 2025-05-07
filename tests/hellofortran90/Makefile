# Fortran compiler
FC = gfortran
FFLAGS = -g

# Source files
SRC = chess.f90 board_utils.f90 chess_types.f90 evaluation.f90 make_unmake.f90 move_generation.f90 search.f90
OBJ = $(SRC:.f90=.o)

# Executable
EXE = chess

all: $(EXE)

$(EXE): $(OBJ)
	$(FC) $(FFLAGS) -o $@ $^

# Object file dependencies (based on which modules each file USEs)
chess.o: chess_types.o board_utils.o evaluation.o make_unmake.o move_generation.o search.o
board_utils.o: chess_types.o
evaluation.o: chess_types.o board_utils.o
make_unmake.o: chess_types.o board_utils.o
move_generation.o: chess_types.o board_utils.o
search.o: chess_types.o board_utils.o move_generation.o make_unmake.o evaluation.o

# Compile rule
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

clean:
	rm -f *.o *.mod $(EXE)

