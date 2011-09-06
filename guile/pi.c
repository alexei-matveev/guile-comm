#include <mpi.h>
#include <assert.h>
// #include <stdio.h>

double pi (MPI_Comm world, int n)
{
    int rank, size, rc;

    rc = MPI_Comm_size (world, &size);
    assert(MPI_SUCCESS==rc);

    rc = MPI_Comm_rank (world, &rank);
    assert(MPI_SUCCESS==rc);

    double h = 1.0 / n;
    double s = 0.0;
    int i;
    for (i = rank; i < n; i += size) {
        double x = h * (i + 0.5);
        s += 4.0 / (1.0 + x * x);
    };

    double partial = s * h;
    double sum;
    rc = MPI_Allreduce (&partial, &sum, 1, MPI_DOUBLE, MPI_SUM, world);
    assert(MPI_SUCCESS==rc);

    // printf("%d of %d pi = %f computed from %d terms\n", rank, size, sum, n);

    return sum;
}

