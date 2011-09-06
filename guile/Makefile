CFLAGS=-Wall
LIBS=`pkg-config --libs guile-1.8`

all: guile-mpi

guile-mpi: guile-mpi.o libguile-comm.o pi.o
	mpicc $(CFLAGS) -o $(@) $(^) $(LIBS)

%.o: %.c
	mpicc $(CFLAGS) -c $(<)

%.so: %.c
	mpicc $(CFLAGS) -shared -o $(@) -fPIC $(<)


clean:
	rm -f *.o *.so
