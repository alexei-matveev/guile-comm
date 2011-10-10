
-include ../machine.inc

# CC = mpicc
CFLAGS=-Wall
LIBS=`pkg-config --libs guile-1.8`

all: guile-mpi

libguile-comm-impl.o = libguile-comm-fint.o
#ibguile-comm-impl.o = libguile-comm-smob.o

libguile-comm.a: $(libguile-comm-impl.o) libguile-comm.o pi.o
	$(AR) ruv $@  $(^)
	$(RANLIB) $@

guile-mpi: guile-mpi.o libguile-comm.a
	$(CC) $(CFLAGS) -o $(@) $(^) $(LIBS)

%.o: %.c
	$(CC) $(CFLAGS) -c $(<)

%.so: %.c
	$(CC) $(CFLAGS) -shared -o $(@) -fPIC $(<)


clean:
	rm -f *.o *.so *.a
