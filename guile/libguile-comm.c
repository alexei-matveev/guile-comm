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
// Try exposing MPI_COMM_WORLD:
//
static SCM comm_world;

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

    // there is only one so far:
    // NO MORE: assert(MPI_COMM_WORLD==ptr->comm);

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
    // NO MORE: assert(MPI_COMM_WORLD==comm);

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

//
// Set a name on a communicator:
//
SCM comm_set_name (SCM world, SCM name)
{
    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = comm_t_comm (world);

    // some communicators have names associated with them:
    char cname[MPI_MAX_OBJECT_NAME];

    // does not null-terninate:
    int len = scm_to_locale_stringbuf (name, cname, MPI_MAX_OBJECT_NAME);

    if ( len > MPI_MAX_OBJECT_NAME )
        len = MPI_MAX_OBJECT_NAME;

    cname[len] = 0;

    int ierr = MPI_Comm_set_name(comm, cname);
    assert(MPI_SUCCESS==ierr);

    return scm_from_int (len);
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
    assert(MPI_SUCCESS==ierr);

    return scm_from_int (ierr);
}

SCM comm_rank (SCM world) // MPI_Comm_rank (world, ...)
{
    int rank;

    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = comm_t_comm (world);

    int ierr = MPI_Comm_rank (comm, &rank);
    assert(MPI_SUCCESS==ierr);

    return scm_from_int (rank);
}

SCM comm_size (SCM world) // MPI_Comm_size (world, ...)
{
    int size;

    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = comm_t_comm (world);

    int ierr = MPI_Comm_size (comm, &size);
    assert(MPI_SUCCESS==ierr);

    return scm_from_int (size);
}

SCM comm_barrier (SCM world) // MPI_Barrier (world, ...)
{
    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = comm_t_comm (world);

    int ierr = MPI_Barrier (comm);
    assert(MPI_SUCCESS==ierr);

    return scm_from_int (ierr);
}

SCM comm_send_recv (SCM world, SCM dst, SCM src, SCM tag, SCM data) // MPI_Sendrecv, note argument order
{
    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = comm_t_comm (world);

    int idst = scm_to_int (dst);
    int isrc = scm_to_int (src);

    // FIXME: the same tag for send and recv:
    int itag = scm_to_int (tag);

    // FIXME: so far only works for ints:
    int sendbuf = scm_to_int (data);
    int recvbuf;
    MPI_Status stat;

    int ierr = MPI_Sendrecv (&sendbuf, 1, MPI_INT, idst, itag, \
                             &recvbuf, 1, MPI_INT, isrc, itag, \
                             comm, &stat);
    assert(MPI_SUCCESS==ierr);

    return scm_from_int (recvbuf);
}

//
// Ugly abstraction:
//
SCM comm_send (SCM world, SCM dest, SCM tag, SCM data) // MPI_Send, note argument order
{
    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = comm_t_comm (world);

    int idest = scm_to_int (dest);
    int itag = scm_to_int (tag);

    // FIXME: so far only works for ints:
    int idata = scm_to_int (data);

    int ierr = MPI_Send (&idata, 1, MPI_INT, idest, itag, comm);
    assert(MPI_SUCCESS==ierr);

    return scm_from_int (ierr);
}

//
// Ugly abstraction:
//
SCM comm_recv (SCM world, SCM source, SCM tag) // MPI_Recv
{
    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = comm_t_comm (world);

    int isource = scm_to_int (source);
    int itag = scm_to_int (tag);

    // FIXME: so far only excpecting ints:
    int idata;
    MPI_Status stat;
    int ierr = MPI_Recv (&idata, 1, MPI_INT, isource, itag, comm, &stat);
    assert(MPI_SUCCESS==ierr);

    return scm_from_int (idata);
}

SCM comm_split (SCM world, SCM color) // MPI_Comm_split (world, color, ...)
{
    int ierr;

    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = comm_t_comm (world);

    // color will define the coutries:
    int icolor = scm_to_int (color);

    // key defines the rank assignment within the country,
    // use world ranks for that:
    int key;
    ierr = MPI_Comm_rank (comm, &key);
    assert(MPI_SUCCESS==ierr);

    MPI_Comm country; // part of the world of the same color

    ierr = MPI_Comm_split (comm, icolor, key, &country);
    assert(MPI_SUCCESS==ierr);

    return comm_t_make(country);
}

//
// I am afraid we cannot delegate freeing communicators
// to garbage collector. Do it explicitly:
//
SCM comm_free (SCM world) // MPI_Comm_free
{
    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = comm_t_comm (world);

    int ierr = MPI_Comm_free (&comm);
    assert(MPI_SUCCESS==ierr);

    return scm_from_int (ierr);
}

//
// double comm_pi (MPI_Comm world, int n);
//
SCM comm_pi (SCM world, SCM n)
{
    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = comm_t_comm (world);

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
    scm_c_define_gsubr ("comm-barrier", 1, 0, 0, comm_barrier);
    scm_c_define_gsubr ("comm-send-recv", 5, 0, 0, comm_send_recv);
    scm_c_define_gsubr ("comm-send", 4, 0, 0, comm_send);
    scm_c_define_gsubr ("comm-recv", 3, 0, 0, comm_recv);
    scm_c_define_gsubr ("comm-split", 2, 0, 0, comm_split);
    scm_c_define_gsubr ("comm-free", 1, 0, 0, comm_free);
    scm_c_define_gsubr ("comm-set-name", 2, 0, 0, comm_set_name);
    scm_c_define_gsubr ("comm-pi", 2, 0, 0, comm_pi);

    // constants and variables:
    comm_world = scm_permanent_object (scm_c_define ("comm-world", comm_t_make (MPI_COMM_WORLD)));
}
