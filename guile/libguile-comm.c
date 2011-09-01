#include <libguile.h>
#include <mpi.h>
#include <assert.h>

SCM comm_init (SCM args) // MPI_Init
{
  int ierr, argc;
  char **argv;

  argv = scm_i_allocate_string_pointers (args);

  // count number of arguments:
  for (argc = 0; argv[argc]; argc++)
    ;

  ierr = MPI_Init (&argc, &argv);

  return scm_from_int (ierr);
}

SCM comm_finalize (void) // MPI_Finalize
{
  int ierr;

  ierr = MPI_Finalize ();

  return scm_from_int (ierr);
}

SCM comm_rank (void) // MPI_Comm_rank(MPI_COMM_WORLD, ...)
{
  int rank, ierr;

  ierr = MPI_Comm_rank (MPI_COMM_WORLD, &rank);
  assert(MPI_SUCCESS==ierr);

  return scm_from_int (rank);
}

SCM comm_size (void) // MPI_Comm_size(MPI_COMM_WORLD, ...)
{
  int size, ierr;

  ierr = MPI_Comm_size (MPI_COMM_WORLD, &size);
  assert(MPI_SUCCESS==ierr);

  return scm_from_int (size);
}

void init_guile_comm (void)
{
  scm_c_define_gsubr ("comm-init", 1, 0, 0, comm_init);
  scm_c_define_gsubr ("comm-finalize", 0, 0, 0, comm_finalize);
  scm_c_define_gsubr ("comm-rank", 0, 0, 0, comm_rank);
  scm_c_define_gsubr ("comm-size", 0, 0, 0, comm_size);
}
