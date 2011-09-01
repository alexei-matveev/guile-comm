LIBS=`pkg-config --libs guile-1.8`

all: guile-mpi

guile-mpi: guile-mpi.o libguile-comm.o
	mpicc -o $(@) $(^) $(LIBS)

%.o: %.c
	mpicc -c $(<)

%.so: %.c
	mpicc -shared -o $(@) -fPIC $(<)


clean:
	rm -f *.o *.so
