#include <libguile.h>
#include <mpi.h>
#include <assert.h>

// example parallel code:
#include "pi.h"

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
static
SCM comm_t_make (const MPI_Comm comm) // comm-init wants to return MPI_COMM_WORLD
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
static
MPI_Comm comm_t_comm (const SCM smob)
{
    scm_assert_smob_type (comm_t_tag, smob);

    struct comm_t *ptr = (struct comm_t *) SCM_SMOB_DATA (smob);

    return ptr->comm;
}

//
// Not using custom print of comm_t SMOBs prints #<comm ...>
// with a "random" ID inside
//
static int
comm_t_print (SCM world, SCM port, scm_print_state *pstate)
{
    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = comm_t_comm (world);

    // there is only one:
    assert(MPI_COMM_WORLD==comm);

    // some communicators have names associated with them:
    char name[MPI_MAX_OBJECT_NAME];
    int len;

    int ierr = MPI_Comm_get_name(comm, name, &len);
    assert(MPI_SUCCESS==ierr);

    scm_puts ("#<comm ", port);
    scm_puts (name, port);
    scm_puts (">", port);

    // non-zero means success:
    return 1;
}

SCM comm_init (SCM args) // MPI_Init
{
    int argc;
    char **argv;

    argv = scm_i_allocate_string_pointers (args);

    // count number of arguments:
    for (argc = 0; argv[argc]; argc++)
        ;

    int ierr = MPI_Init (&argc, &argv);
    assert(MPI_SUCCESS==ierr);

    // return scm_from_int (ierr);
    return comm_t_make(MPI_COMM_WORLD);
}

SCM comm_finalize (void) // MPI_Finalize
{
  int ierr = MPI_Finalize ();

  return scm_from_int (ierr);
}

SCM comm_rank (SCM world) // MPI_Comm_rank(world, ...)
{
    int rank;

    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = comm_t_comm (world);

    // there is only one so far:
    assert(MPI_COMM_WORLD==comm);

    int ierr = MPI_Comm_rank (comm, &rank);
    assert(MPI_SUCCESS==ierr);

    return scm_from_int (rank);
}

SCM comm_size (SCM world) // MPI_Comm_size(world, ...)
{
    int size;

    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = comm_t_comm (world);

    // there is only one so far:
    assert(MPI_COMM_WORLD==comm);

    int ierr = MPI_Comm_size (comm, &size);
    assert(MPI_SUCCESS==ierr);

    return scm_from_int (size);
}

//
// double comm_pi (MPI_Comm world, int n);
//
SCM comm_pi (SCM world, SCM n)
{
    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = comm_t_comm (world);

    // there is only one so far:
    assert(MPI_COMM_WORLD==comm);

    int N = scm_to_int (n);

    // compute PI in parallel:
    double dbl = pi (comm, N);

    return scm_from_double (dbl);
}

void init_guile_comm (void)
{
    comm_t_tag = scm_make_smob_type ("comm", sizeof (struct comm_t));
    /*
    scm_set_smob_mark (comm_t_tag, comm_t_mark);
    scm_set_smob_free (comm_t_tag, comm_t_free);
    */
    scm_set_smob_print (comm_t_tag, comm_t_print);

    scm_c_define_gsubr ("comm-init", 1, 0, 0, comm_init);
    scm_c_define_gsubr ("comm-finalize", 0, 0, 0, comm_finalize);
    scm_c_define_gsubr ("comm-rank", 1, 0, 0, comm_rank);
    scm_c_define_gsubr ("comm-size", 1, 0, 0, comm_size);
    scm_c_define_gsubr ("comm-pi", 2, 0, 0, comm_pi);
}
