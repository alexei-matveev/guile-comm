#include <libguile.h>
#include <mpi.h>
#include <assert.h>
// #include <stdio.h>

// also declares wrappers for communicator smob:
#include "libguile-comm.h"

// example parallel code:
#include "pi.h"

#define MAX_BUF_LENGTH 512

//
// size_t varies between 32/64 platforms, will be redefined later:
//
static MPI_Datatype MPI_SIZE_T = MPI_UNSIGNED;

// FIXME: make Intel icc understand this:
//#if sizeof (size_t) == sizeof (unsigned long)
//#define MPI_SIZE_T MPI_UNSIGNED_LONG
//#elif sizeof (size_t) == sizeof (unsigned int)
//#define MPI_SIZE_T MPI_UNSIGNED
//#else
//#error "unknown size_t"
//#endif

static SCM object_to_string (SCM obj);
static SCM string_to_object (SCM obj);

static char *scm_to_byte_string (SCM obj, size_t *lenp);
static SCM scm_from_byte_string (const char *buf, size_t len);

//
// Try exposing MPI_COMM_WORLD:
//
// static SCM guile_comm_world;

//
// Set a name on a communicator:
//
SCM
guile_comm_set_name (SCM world, SCM name)
{
    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = scm_to_comm (world);

    // some communicators have names associated with them:
    char cname[MPI_MAX_OBJECT_NAME];

    // does not null-terninate:
    int len = scm_to_locale_stringbuf (name, cname, MPI_MAX_OBJECT_NAME);

    if ( len > MPI_MAX_OBJECT_NAME )
        len = MPI_MAX_OBJECT_NAME;

    cname[len] = '\0';

    int ierr = MPI_Comm_set_name (comm, cname);
    assert (MPI_SUCCESS==ierr);

    return scm_from_int (len);
}

SCM
guile_comm_init (SCM args) // MPI_Init
{
    int argc;
    char **argv;

    argv = scm_i_allocate_string_pointers (args);

    // count number of arguments:
    for (argc = 0; argv[argc]; argc++)
        ;

    int ierr = MPI_Init (&argc, &argv);
    assert (MPI_SUCCESS==ierr);

    // return scm_from_int (ierr);
    return scm_from_comm (MPI_COMM_WORLD);
}

SCM
guile_comm_finalize (void) // MPI_Finalize
{
    int ierr = MPI_Finalize ();
    assert (MPI_SUCCESS==ierr);

    return scm_from_int (ierr);
}

SCM
guile_comm_rank (SCM world) // MPI_Comm_rank (world, ...)
{
    int rank;

    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = scm_to_comm (world);

    int ierr = MPI_Comm_rank (comm, &rank);
    assert (MPI_SUCCESS==ierr);

    return scm_from_int (rank);
}

SCM
guile_comm_size (SCM world) // MPI_Comm_size (world, ...)
{
    int size;

    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = scm_to_comm (world);

    int ierr = MPI_Comm_size (comm, &size);
    assert (MPI_SUCCESS==ierr);

    return scm_from_int (size);
}

SCM
guile_comm_barrier (SCM world) // MPI_Barrier (world, ...)
{
    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = scm_to_comm (world);

    int ierr = MPI_Barrier (comm);
    assert (MPI_SUCCESS==ierr);

    return scm_from_int (ierr);
}

SCM
guile_comm_bcast (const SCM world, const SCM root, const SCM obj) // MPI_Bcast, note argument order
{
    size_t len;
    char *sendbuf;
    char recvbuf[MAX_BUF_LENGTH];

    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = scm_to_comm (world);

    int iroot = scm_to_int (root);

    int rank;
    int ierr = MPI_Comm_rank (comm, &rank);
    assert (MPI_SUCCESS==ierr);

    if ( rank == iroot ) {
        // searialize the object, dont forget to free() later:
        sendbuf = scm_to_byte_string (obj, &len);
        // FIXME: recv buffer has finite length:
        assert (len <= MAX_BUF_LENGTH);
    }

    // Broadcast the size, or should we always send MAX_BUF_LENGTH?
    ierr = MPI_Bcast (&len, 1, MPI_SIZE_T, iroot, comm);
    assert (MPI_SUCCESS==ierr);

    // FIXME: recv buffer has finite length:
    assert (len <= MAX_BUF_LENGTH);

    if ( rank == iroot ) {
        ierr = MPI_Bcast ( sendbuf, len, MPI_CHAR, iroot, comm);
        assert (MPI_SUCCESS==ierr);
        free (sendbuf);
    } else {
        ierr = MPI_Bcast (&recvbuf, len, MPI_CHAR, iroot, comm);
        assert (MPI_SUCCESS==ierr);
    }

    if ( rank == iroot ) {
        // FIXME: should we return a copy instead?
        return obj;
    } else {
        return scm_from_byte_string (recvbuf, len);
    }
}

SCM
guile_comm_send_recv (SCM world, SCM dst, SCM src, SCM tag, SCM obj) // MPI_Sendrecv, note argument order
{
    size_t len;
    char *sendbuf;
    char recvbuf[MAX_BUF_LENGTH];

    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = scm_to_comm (world);

    int idst = scm_to_int (dst);
    int isrc = scm_to_int (src);

    // FIXME: the same tag for send and recv:
    int itag = scm_to_int (tag);

    // searialize the object, dont forget to free() later:
    sendbuf = scm_to_byte_string (obj, &len);
    assert (len <= MAX_BUF_LENGTH); // here: <=

    MPI_Status stat;

    // send just enough elements:
    int ierr = MPI_Sendrecv ( sendbuf,            len, MPI_CHAR, idst, itag, \
                             &recvbuf, MAX_BUF_LENGTH, MPI_CHAR, isrc, itag, \
                             comm, &stat);
    assert (MPI_SUCCESS==ierr);

    free (sendbuf);

    // get the size of the received data:
    int ilen;
    ierr = MPI_Get_count (&stat, MPI_CHAR, &ilen);
    assert (MPI_SUCCESS==ierr);
    assert (ilen <= MAX_BUF_LENGTH); // redundant, as MPI would fail

    return scm_from_byte_string (recvbuf, ilen);
}

//
// Send as a procedure, receive as a function that returns
// arbitrary types unrelated to input is an ugly abstraction:
//
SCM
guile_comm_send (SCM world, SCM dst, SCM tag, SCM obj)
{
    size_t len;
    char *buf;

    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = scm_to_comm (world);

    int idst = scm_to_int (dst);
    int itag = scm_to_int (tag);

    // searialize the object, dont forget to free() later:
    buf = scm_to_byte_string (obj, &len);
    assert (len < MAX_BUF_LENGTH);

    // printf ("SEND:%s\n", buf);

    // send just enough elements:
    int ierr = MPI_Send (buf, len, MPI_CHAR, idst, itag, comm);
    assert (MPI_SUCCESS==ierr);

    free (buf);

    return scm_from_int (ierr);
}

SCM
guile_comm_recv (SCM world, SCM src, SCM tag)
{
    char buf[MAX_BUF_LENGTH];

    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = scm_to_comm (world);

    int isrc = scm_to_int (src);
    int itag = scm_to_int (tag);

    MPI_Status stat;

    int ierr = MPI_Recv (buf, MAX_BUF_LENGTH, MPI_CHAR, isrc, itag, comm, &stat);
    assert (MPI_SUCCESS==ierr);

    // printf ("RECV:%s\n", buf);

    // get the size of the received data:
    int ilen;
    ierr = MPI_Get_count (&stat, MPI_CHAR, &ilen);
    assert (MPI_SUCCESS==ierr);

    return scm_from_byte_string (buf, ilen);
}

SCM
guile_comm_split (SCM world, SCM color) // MPI_Comm_split (world, color, ...)
{
    int ierr;

    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = scm_to_comm (world);

    // color will define the coutries:
    int icolor = scm_to_int (color);

    // key defines the rank assignment within the country,
    // use world ranks for that:
    int key;
    ierr = MPI_Comm_rank (comm, &key);
    assert (MPI_SUCCESS==ierr);

    MPI_Comm country; // part of the world of the same color

    ierr = MPI_Comm_split (comm, icolor, key, &country);
    assert (MPI_SUCCESS==ierr);

    return scm_from_comm (country);
}

//
// I am afraid we cannot delegate freeing communicators
// to garbage collector. Do it explicitly:
//
SCM
guile_comm_free (SCM world) // MPI_Comm_free
{
    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = scm_to_comm (world);

    int ierr = MPI_Comm_free (&comm);
    assert (MPI_SUCCESS==ierr);

    return scm_from_int (ierr);
}

//
// double guile_comm_pi (MPI_Comm world, int n);
//
SCM
guile_comm_pi (SCM world, SCM n)
{
    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = scm_to_comm (world);

    int N = scm_to_int (n);

    // compute PI in parallel:
    double dbl = pi (comm, N);

    return scm_from_double (dbl);
}

//
// These write/read scheme objects to scheme strings:
//
static SCM
string_to_object (SCM str)
{
    SCM port = scm_open_input_string (str);

    SCM obj = scm_read (port);

    scm_close_port (port);

    return obj;
}

static SCM
object_to_string (SCM obj) // variant 2
{
    SCM str = scm_object_to_string (obj, SCM_UNDEFINED);

    return str;
}

//
// These serialize/deserialize objects to char buffers.
// Dont forget to free() the result of scm_to_byte_string()
// when finished:
//
static char *
scm_to_byte_string (SCM obj, size_t *lenp)
{
    SCM str = object_to_string (obj);

    return scm_to_locale_stringn (str, lenp);
}

static SCM
scm_from_byte_string (const char *buf, size_t len)
{
    SCM str = scm_from_locale_stringn (buf, len);

    return string_to_object (str);
}

void init_guile_comm (void)
{
    //
    // size_t varies between 32/64 platforms, set MPI_SIZE_T here:
    //
    if (sizeof (size_t) == sizeof (unsigned long)) {
        MPI_SIZE_T = MPI_UNSIGNED_LONG;
    } else if ( sizeof (size_t) == sizeof (unsigned int)) {
        MPI_SIZE_T = MPI_UNSIGNED;
    } else {
        assert (0);
    }
    init_guile_comm_smob();

    scm_c_define_gsubr ("comm-init", 1, 0, 0, guile_comm_init);
    scm_c_define_gsubr ("comm-finalize", 0, 0, 0, guile_comm_finalize);
    scm_c_define_gsubr ("comm-rank", 1, 0, 0, guile_comm_rank);
    scm_c_define_gsubr ("comm-size", 1, 0, 0, guile_comm_size);
    scm_c_define_gsubr ("comm-barrier", 1, 0, 0, guile_comm_barrier);

    scm_c_define_gsubr ("comm-bcast", 3, 0, 0, guile_comm_bcast);

    scm_c_define_gsubr ("comm-send-recv", 5, 0, 0, guile_comm_send_recv);
    scm_c_define_gsubr ("comm-send", 4, 0, 0, guile_comm_send);
    scm_c_define_gsubr ("comm-recv", 3, 0, 0, guile_comm_recv);

    scm_c_define_gsubr ("comm-split", 2, 0, 0, guile_comm_split);
    scm_c_define_gsubr ("comm-free", 1, 0, 0, guile_comm_free);
    scm_c_define_gsubr ("comm-set-name", 2, 0, 0, guile_comm_set_name);
    scm_c_define_gsubr ("comm-pi", 2, 0, 0, guile_comm_pi);

    // scm_c_define_gsubr ("object-to-string", 1, 0, 0, object_to_string);
    // scm_c_define_gsubr ("string-to-object", 1, 0, 0, string_to_object);

    // constants and variables:
    // guile_comm_world = scm_permanent_object (scm_c_define ("comm-world", scm_from_comm (MPI_COMM_WORLD)));
}
