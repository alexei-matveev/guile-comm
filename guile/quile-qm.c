#include <stdlib.h>
#include <libguile.h>

// run paragauss by calling these in sequence:
void qm_init(void);
void qm_run(void);
void qm_finalize(void);

static SCM
guile_qm_init(void)
{
    qm_init ();
    return SCM_UNSPECIFIED;
}

static SCM
guile_qm_run(void)
{
    qm_run ();
    return SCM_UNSPECIFIED;
}

static SCM
guile_qm_finalize(void)
{
    qm_finalize ();
    return SCM_UNSPECIFIED;
}

static void
guile_main (void *data, int argc, char **argv)
{
    scm_c_define_gsubr ("qm-init", 0, 0, 0, guile_qm_init);
    scm_c_define_gsubr ("qm-run", 0, 0, 0, guile_qm_run);
    scm_c_define_gsubr ("qm-finalize", 0, 0, 0, guile_qm_finalize);

    scm_shell (argc, argv); // never returns
}

int
main (int argc, char **argv)
{
  // run ();

  // void scm_boot_guile (int argc, char **argv, void (*main_func) (void *data, int argc, char **argv), void *data)
  scm_boot_guile (argc, argv, guile_main, 0);
  return 0; /* never reached */
}
