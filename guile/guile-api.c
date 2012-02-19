#include <libguile.h>

int guile_macro_scm_to_int (SCM obj);
int guile_macro_scm_is_true (SCM obj);

int guile_macro_scm_to_int (SCM obj)
{
  return scm_to_int (obj);
}

int guile_macro_scm_is_true (SCM obj)
{
  return scm_is_true (obj);
}
