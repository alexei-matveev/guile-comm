#include <libguile.h>

SCM guile_macro_scm_from_int (int i);
int guile_macro_scm_to_int (SCM obj);

int (scm_is_true) (SCM obj);
int (scm_is_symbol) (SCM obj);

int (scm_is_null) (SCM obj);

// FIXME: this one returns a constant:
SCM scm_eol (void);

SCM guile_macro_scm_from_int (int i)
{
  return scm_from_int(i);
}

int guile_macro_scm_to_int (SCM obj)
{
  return scm_to_int(obj);
}

int (scm_is_true) (SCM obj)
{
  return scm_is_true(obj);
}

int (scm_is_symbol) (SCM obj)
{
  return scm_is_symbol(obj);
}

int (scm_is_null) (SCM obj)
{
  return scm_is_null(obj);
}

SCM scm_eol ()
{
  return SCM_EOL;
}
