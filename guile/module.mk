#
# Make has no scopes, not to pollute the namespace
# use $(GUILE)/..., or GUILE- prefix for targets and $(GUILE-...)
# prefix for variables. A notable exception so far are the
# top-level tragets
#
#       $(libguile-comm.a)
#       $(guile-qm.o)
#

#
# We expect the before "include"ing this file the
# variable $(GUILE) is set to a suitable prefix.
#
# CURDIR is set on every make or $(MAKE) -C dir:
#
ifndef GUILE
GUILE = $(CURDIR)
endif

#
# These may be used to refer to the targets from outside:
#
libguile-comm.a = $(GUILE)/libguile-comm.a
guile-qm.o = $(GUILE)/guile-qm.o

#
# One implementation uses Fortran integers for communicators,
# another (more complicated) uses the C MPI_Comm wrapped into
# SMOB:
#
GUILE-libguile-comm-impl.o = $(GUILE)/libguile-comm-fint.o
#UILE-libguile-comm-impl.o = $(GUILE)/libguile-comm-smob.o

GUILE-libguile-comm-objs = \
        $(GUILE)/libguile-comm.o \
        $(GUILE-libguile-comm-impl.o) \
        $(GUILE)/pi.o \

GUILE-objs = $(GUILE-libguile-comm-objs) \
        $(guile-qm.o)

$(libguile-comm.a): $(GUILE-libguile-comm-objs)
	$(AR) ruv $@  $(^)
	$(RANLIB) $@

GUILE-clean:
	rm -f $(libguile-comm.a)

.PHONY: GUILE-clean

#
# Below we modify "global" variables and prerequisites of
# top-level targets ...
#

#
# This (global) variable (cobjs) is used in Make.rules to build and
# include dependencies:
#
cobjs += $(GUILE-objs)

#
# This is also a top-level (global) target, extend the list
# of dependencies:
#
clean: GUILE-clean
