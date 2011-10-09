#include <libguile.h>
#include <mpi.h>

#include "libguile-comm.h"

//
// Here we use the one-to-one relation between MPI_Comm instances
// and their integer Fortran representations of type MPI_Fint
//

//
// This converts an MPI_Comm to a SCM int:
//
SCM scm_from_comm (const MPI_Comm comm) // comm-init wants to return MPI_COMM_WORLD
{
    return scm_from_int (MPI_Comm_c2f (comm));
}

//
// This converts a SCM int to an MPI_Comm:
//
MPI_Comm scm_to_comm (const SCM comm)
{
    return MPI_Comm_f2c (scm_to_int (comm));
}

void init_guile_comm_smob (void)
{
    // nothing
}
