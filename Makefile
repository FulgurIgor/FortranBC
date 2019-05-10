FC    = gfortran
STD   = -std=f2003
EXE   = a.out

all:
	$(FC) main.F90 $(STD) -o $(EXE)
	rm -f *.mod
	./$(EXE)

help:
	echo "Just do make"

run:
	./$(EXE)

rm:
	rm -f *.mod || true
	rm -f $(EXE)