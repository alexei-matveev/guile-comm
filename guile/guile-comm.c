#include <stdlib.h>
#include <libguile.h>
#include <mpi.h>
#include "libguile-comm.h"

static void
inner_main (void *data, int argc, char **argv)
{
  /*
  int rank;
  MPI_Init (&argc, &argv);
  MPI_Comm_rank (MPI_COMM_WORLD, &rank);

  printf("rank = %d (before)\n", rank);
  */

  /*
   *  Intialize libguile-comm module, this does not yet call
   *  MPI_Init (FIXME: should we?). Instead issue
   *
   *     (comm-init (command-line))
   *     (comm-finalize)
   *
   *  in Scheme code.
   */
  guile_comm_module_init ();

  scm_shell (argc, argv); // never returns

  /*
  printf("rank = %d (after)\n", rank);

  // FIXME: needs to be called, but is unreachable:
  MPI_Finalize ();
  */
}

int
main (int argc, char **argv)
{
  /*
  int flag = 0;

  // to make sure MPI is linked in:
  MPI_Initialized (&flag);
  */

  // void scm_boot_guile (int argc, char **argv, void (*main_func) (void *data, int argc, char **argv), void *data)
  scm_boot_guile (argc, argv, inner_main, 0);
  return 0; /* never reached */
}
