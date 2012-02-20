#include <libguile.h>

int guile_macro_scm_to_int (SCM obj);
int guile_macro_scm_is_true (SCM obj);
int guile_macro_scm_is_symbol (SCM obj);
int guile_macro_scm_is_null (SCM obj);
SCM guile_macro_scm_eol (void);

int guile_macro_scm_to_int (SCM obj)
{
  return scm_to_int (obj);
}

int guile_macro_scm_is_true (SCM obj)
{
  return scm_is_true (obj);
}

int guile_macro_scm_is_symbol (SCM obj)
{
  return scm_is_symbol (obj);
}

int guile_macro_scm_is_null (SCM obj)
{
  return scm_is_null (obj);
}

SCM guile_macro_scm_eol ()
{
  return SCM_EOL;
}
