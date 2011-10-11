#include <stdlib.h>
#include <libguile.h>
#include <mpi.h>
#include <assert.h>

//
// Here we assume that communicators are implemented
// in fortran style as SCM integers, libguile-comm should
// be consistent to interoperate:
//
#include "libguile-comm.h"

// run paragauss by calling these in sequence:
int qm_init(void);
void qm_run(int world);
void qm_finalize(int world);

static SCM
guile_qm_init (void)
{
    int world = qm_init ();
    return scm_from_int (world);
}

static SCM
guile_qm_run (SCM world)
{
    int fworld = scm_to_int (world);
    qm_run (fworld);
    return SCM_UNSPECIFIED;
}

static SCM
guile_qm_finalize (const SCM world)
{
    int fworld = scm_to_int (world);
    qm_finalize (fworld);
    return SCM_UNSPECIFIED;
}

static void
guile_main (void *data, int argc, char **argv)
{
    // defines comm-* gsubrs:
    guile_comm_init_module ();

    scm_c_define_gsubr ("qm-init", 0, 0, 0, guile_qm_init);
    scm_c_define_gsubr ("qm-run", 1, 0, 0, guile_qm_run);
    scm_c_define_gsubr ("qm-finalize", 1, 0, 0, guile_qm_finalize);

    scm_shell (argc, argv); // never returns
}

int
main (int argc, char **argv)
{
  // void scm_boot_guile (int argc, char **argv, void (*main_func) (void *data, int argc, char **argv), void *data)
  scm_boot_guile (argc, argv, guile_main, 0);
  return 0; /* never reached */
}
