#include <stdlib.h>
#include <libguile.h>
#include <mpi.h> /* used in libguile-comm.h */
#include "libguile-comm.h"

static void
inner_main (void *data, int argc, char **argv)
{
  /*
   *  To    intialize    comm     module    one    needs    to    call
   *  guile_comm_module_init  ().  This  function  does not  yet  call
   *  MPI_Init (). Instead it defines comm-init for the user to issue
   *
   *     (comm-init (command-line))
   *     ...
   *     (comm-finalize)
   *
   *  in Scheme code.
   */

  /*
   * Note  that  the names  defined  here  are  put into  the  private
   * namespace  of (guile-user)  module. If  you want  to call  any of
   * these you may need to "steal" it from there by dereferencing them
   * as e.g. (@@ (guile-user) guile-comm-module-init).
   *
   * Calling this will define comm-* gsubrs:
   */
  scm_c_define_gsubr ("guile-comm-module-init", 0, 0, 0, guile_comm_module_init);

  scm_shell (argc, argv); // never returns
}

int
main (int argc, char **argv)
{

  // void scm_boot_guile (int argc, char **argv, void (*main_func) (void *data, int argc, char **argv), void *data)
  scm_boot_guile (argc, argv, inner_main, 0);
  return 0; /* never reached */
}
