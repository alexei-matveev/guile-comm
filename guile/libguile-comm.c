#include <libguile.h>
#include <mpi.h>
#include <assert.h>
// #include <stdio.h>

// also declares wrappers for communicator smob:
#include "libguile-comm.h"

// example parallel code:
#include "pi.h"

#define MAX_BUF_LENGTH 512

static SCM object_to_string (SCM obj);
static SCM string_to_object (SCM obj);

static size_t write_buf (SCM obj, char *buf, size_t max_len);
static SCM read_buf (const char *buf, size_t max_len);

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
guile_comm_send_recv (SCM world, SCM dst, SCM src, SCM tag, SCM obj) // MPI_Sendrecv, note argument order
{
    size_t max_len = MAX_BUF_LENGTH;

    char sendbuf[MAX_BUF_LENGTH];
    char recvbuf[MAX_BUF_LENGTH];

    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = scm_to_comm (world);

    int idst = scm_to_int (dst);
    int isrc = scm_to_int (src);

    // FIXME: the same tag for send and recv:
    int itag = scm_to_int (tag);

    // FIXME: buffer may be too small:
    size_t len = write_buf (obj, sendbuf, max_len);
    assert (len <= max_len); // here: <=

    MPI_Status stat;

    // send just enough elements:
    int ierr = MPI_Sendrecv (&sendbuf,     len, MPI_CHAR, idst, itag, \
                             &recvbuf, max_len, MPI_CHAR, isrc, itag, \
                             comm, &stat);
    assert (MPI_SUCCESS==ierr);

    // get the size of the received data:
    int ilen;
    ierr = MPI_Get_count (&stat, MPI_CHAR, &ilen);
    assert (MPI_SUCCESS==ierr);
    assert (ilen <= max_len); // redundant, as MPI would fail

    return read_buf (recvbuf, ilen);
}

//
// Send as a procedure, receive as a function that returns
// arbitrary types unrelated to input is an ugly abstraction:
//
SCM
guile_comm_send (SCM world, SCM dst, SCM tag, SCM obj)
{
    size_t max_len = MAX_BUF_LENGTH;
    char buf[MAX_BUF_LENGTH];

    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = scm_to_comm (world);

    int idst = scm_to_int (dst);
    int itag = scm_to_int (tag);

    size_t len = write_buf (obj, buf, max_len);
    assert (len<max_len);

    // printf ("SEND:%s\n", buf);

    // send just enough elements:
    int ierr = MPI_Send (buf, len, MPI_CHAR, idst, itag, comm);
    assert (MPI_SUCCESS==ierr);

    return scm_from_int (ierr);
}

SCM
guile_comm_recv (SCM world, SCM src, SCM tag)
{
    size_t max_len = MAX_BUF_LENGTH;
    char buf[MAX_BUF_LENGTH];

    // extract MPI_Comm, verifies the type:
    MPI_Comm comm = scm_to_comm (world);

    int isrc = scm_to_int (src);
    int itag = scm_to_int (tag);

    MPI_Status stat;

    int ierr = MPI_Recv (buf, max_len, MPI_CHAR, isrc, itag, comm, &stat);
    assert (MPI_SUCCESS==ierr);

    // printf ("RECV:%s\n", buf);

    // get the size of the received data:
    int ilen;
    ierr = MPI_Get_count (&stat, MPI_CHAR, &ilen);
    assert (MPI_SUCCESS==ierr);

    return read_buf (buf, ilen);
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
static
SCM
string_to_object (SCM str)
{
    SCM port = scm_open_input_string (str);

    SCM obj = scm_read (port);

    scm_close_port (port);

    return obj;
}

#if 0

static
SCM
object_to_string (SCM obj) // variant 1
{
    SCM port = scm_open_output_string ();

    scm_write (obj, port);

    // result = scm_strport_to_string (port);
    SCM str = scm_get_output_string (port);

    scm_close_port (port);

    return str;
}

#else

static
SCM
object_to_string (SCM obj) // variant 2
{
    SCM str = scm_object_to_string (obj, SCM_UNDEFINED);

    return str;
}

#endif

//
// These serialize/deserialize objects to char buffers
//
// FIXME: both sending/receiving and serializing/deserializing
//        of variable buffer sizes.
//
static
size_t write_buf (SCM obj, char *buf, size_t max_len)
{
    SCM str = object_to_string (obj);

    size_t len = scm_to_locale_stringbuf (str, buf, max_len);

    // NOTE: scm_to_locale_stringbuf () does not put terminating \0
    //       into the character buffer. For printing, and 
    //       later reading in read_buf a \0 must be there:
    assert (len < max_len); // yes, not <=

    // buf[len] = '!';
    // printf ("write_buf:%s\n", buf);
    buf[len] = '\0';

    return len + 1;
}

static SCM
read_buf (const char *buf, size_t max_len)
{
    // we expect here a \0-terminated string:
    SCM str = scm_from_locale_string (buf);

    return string_to_object (str);
}

void init_guile_comm (void)
{
    init_guile_comm_smob();

    scm_c_define_gsubr ("comm-init", 1, 0, 0, guile_comm_init);
    scm_c_define_gsubr ("comm-finalize", 0, 0, 0, guile_comm_finalize);
    scm_c_define_gsubr ("comm-rank", 1, 0, 0, guile_comm_rank);
    scm_c_define_gsubr ("comm-size", 1, 0, 0, guile_comm_size);
    scm_c_define_gsubr ("comm-barrier", 1, 0, 0, guile_comm_barrier);
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
