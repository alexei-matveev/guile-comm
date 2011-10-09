CFLAGS=-Wall
LIBS=`pkg-config --libs guile-1.8`

all: guile-mpi

libguile-comm-impl.o = libguile-comm-fint.o
#ibguile-comm-impl.o = libguile-comm-smob.o

guile-mpi: guile-mpi.o $(libguile-comm-impl.o) libguile-comm.o pi.o
	mpicc $(CFLAGS) -o $(@) $(^) $(LIBS)

%.o: %.c
	mpicc $(CFLAGS) -c $(<)

%.so: %.c
	mpicc $(CFLAGS) -shared -o $(@) -fPIC $(<)


clean:
	rm -f *.o *.so
