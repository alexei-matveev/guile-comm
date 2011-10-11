#include <libguile.h>
#include <mpi.h>
#include <assert.h>

#include "libguile-comm.h"

#define SCM_TO_COMM_UNSAFE(smob) (((struct comm_t *) SCM_SMOB_DATA (smob))->comm)

//
// Guile identifies SMOB (small objects) types by tags:
//
static scm_t_bits comm_t_tag;

//
// Implementation of MPI_Comm may be very different,
// e.g. a struct pointer in OpenMPI or an int in MPICH:
//
struct comm_t {
    MPI_Comm comm;
};

//
// This converts an MPI_Comm to a comm_t SMOB:
//
SCM scm_from_comm (const MPI_Comm comm) // comm-init wants to return MPI_COMM_WORLD
{
    SCM smob;
    struct comm_t *new;

    // Step 1. Allocate the memory block:
    new = (struct comm_t *) scm_gc_malloc (sizeof (struct comm_t), "comm");

    // Step 2. Initialize it with straight code:
    new->comm = comm;

    // Step 3. Create the smob:
    SCM_NEWSMOB (smob, comm_t_tag, new);

    // Step 4. Finish the initialization:
    // ...

    return smob;
}

//
// This converts a comm_t SMOB to an MPI_Comm:
//
MPI_Comm scm_to_comm (const SCM smob)
{
    scm_assert_smob_type (comm_t_tag, smob);

    return SCM_TO_COMM_UNSAFE (smob);
}

//
// Not using custom print of comm_t SMOBs prints #<comm ...>
// with a "random" ID inside
//
static int
comm_t_print (SCM world, SCM port, scm_print_state *pstate)
{
    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = scm_to_comm (world);

    // there is only one:
    // NO MORE: assert (MPI_COMM_WORLD==comm);

    // some communicators have names associated with them:
    char name[MPI_MAX_OBJECT_NAME];
    int len;

    int ierr = MPI_Comm_get_name (comm, name, &len);
    assert (MPI_SUCCESS==ierr);

    scm_puts ("#<comm ", port);
    scm_puts (name, port);
    scm_puts (">", port);

    // non-zero means success:
    return 1;
}

void guile_comm_init_smob (void)
{
    comm_t_tag = scm_make_smob_type ("comm", sizeof (struct comm_t));
    /*
    scm_set_smob_mark (comm_t_tag, comm_t_mark);
    scm_set_smob_free (comm_t_tag, comm_t_free);
    */
    scm_set_smob_print (comm_t_tag, comm_t_print);
}
