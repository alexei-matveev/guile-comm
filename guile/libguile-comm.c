#include <libguile.h>
#include <mpi.h>
#include <assert.h>

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
    struct comm_t *ptr = (struct comm_t *) SCM_SMOB_DATA (smob);

    return ptr->comm;
}

SCM comm_init (SCM args) // MPI_Init
{
    int ierr, argc;
    char **argv;

    argv = scm_i_allocate_string_pointers (args);

    // count number of arguments:
    for (argc = 0; argv[argc]; argc++)
        ;

    ierr = MPI_Init (&argc, &argv);
    assert(MPI_SUCCESS==ierr);

    // return scm_from_int (ierr);
    return comm_t_make(MPI_COMM_WORLD);
}

SCM comm_finalize (void) // MPI_Finalize
{
  int ierr;

  ierr = MPI_Finalize ();

  return scm_from_int (ierr);
}

SCM comm_rank (SCM world) // MPI_Comm_rank(world, ...)
{
    int rank, ierr;

    scm_assert_smob_type (comm_t_tag, world);

    // extract MPI_Comm:
    MPI_Comm comm = comm_t_comm (world);

    // there is only one so far:
    assert(comm==MPI_COMM_WORLD);

    ierr = MPI_Comm_rank (comm, &rank);
    assert(MPI_SUCCESS==ierr);

    return scm_from_int (rank);
}

SCM comm_size (SCM world) // MPI_Comm_size(world, ...)
{
    int size, ierr;

    scm_assert_smob_type (comm_t_tag, world);

    // extract MPI_Comm:
    MPI_Comm comm = comm_t_comm (world);

    // there is only one so far:
    assert(comm==MPI_COMM_WORLD);

    ierr = MPI_Comm_size (comm, &size);
    assert(MPI_SUCCESS==ierr);

    return scm_from_int (size);
}

void init_guile_comm (void)
{
    comm_t_tag = scm_make_smob_type ("comm", sizeof (struct comm_t));
    /*
    scm_set_smob_mark (comm_t_tag, comm_t_mark);
    scm_set_smob_free (comm_t_tag, comm_t_free);
    scm_set_smob_print (comm_t_tag, comm_t_print);
    */

    scm_c_define_gsubr ("comm-init", 1, 0, 0, comm_init);
    scm_c_define_gsubr ("comm-finalize", 0, 0, 0, comm_finalize);
    scm_c_define_gsubr ("comm-rank", 1, 0, 0, comm_rank);
    scm_c_define_gsubr ("comm-size", 1, 0, 0, comm_size);
}
