module scm

use iso_c_binding
implicit none
private

interface
   !
   ! No object satises more than one of the following predicates:
   !
   ! boolean?  pair?  symbol?  number?  char?  string?  vector?
   ! bytevector?  port? procedure?  null?
   !
   function scm_symbol_p (object) result (yes) bind (c)
     !
     ! SCM scm_symbol_p (SCM obj)
     !
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: object
     type(c_ptr) :: yes
   end function scm_symbol_p

   function scm_string_p (object) result (yes) bind (c)
     !
     ! SCM scm_string_p (SCM obj)
     !
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: object
     type(c_ptr) :: yes
   end function scm_string_p

   function scm_number_p (object) result (yes) bind (c)
     !
     ! SCM scm_number_p (SCM obj)
     !
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: object
     type(c_ptr) :: yes
   end function scm_number_p

   function scm_pair_p (object) result (yes) bind (c)
     !
     ! SCM scm_pair_p (SCM obj)
     !
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: object
     type(c_ptr) :: yes
   end function scm_pair_p

   function scm_null_p (object) result (yes) bind (c)
     !
     ! SCM scm_null_p (SCM x)
     !
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: object
     type(c_ptr) :: yes
   end function scm_null_p

   !
   ! These may be usefull to tell appart the instances of of number?:
   !
   function scm_exact_p (object) result (yes) bind (c)
     !
     ! SCM scm_exact_p (SCM x)
     !
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: object
     type(c_ptr) :: yes
   end function scm_exact_p

   function scm_inexact_p (object) result (yes) bind (c)
     !
     ! SCM scm_inexact_p (SCM x)
     !
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: object
     type(c_ptr) :: yes
   end function scm_inexact_p

   !
   ! Type constructors/accessors, integers, doubles, strings,
   ! logicals, lists:
   !

   !
   ! FIXME: Guile API exports a macro for this, we added a function,
   ! see guile-api.c:
   !
   function guile_macro_scm_is_true (object) result (yes) bind (c)
     !
     ! int scm_is_true (SCM obj)
     !
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: object
     integer(c_int) :: yes
   end function guile_macro_scm_is_true

   function scm_from_int (i) result (exact) bind (c)
     !
     ! SCM scm_from_int (int i)
     !
     use iso_c_binding
     implicit none
     integer(c_int), intent(in), value :: i
     type(c_ptr) :: exact
   end function scm_from_int

   !
   ! FIXME: Guile API exports a macro for this, see guile-api.c:
   !
   function scm_to_int (exact) result (i) bind (c, name="guile_macro_scm_to_int")
     !
     ! int scm_from_int (SCM exact)
     !
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: exact
     integer(c_int) :: i
   end function scm_to_int

   function scm_from_double (d) result (inexact) bind (c)
     !
     ! SCM scm_from_double (double d)
     !
     use iso_c_binding
     implicit none
     real(c_double), intent(in), value :: d
     type(c_ptr) :: inexact
   end function scm_from_double

   function scm_to_double (inexact) result (d) bind (c)
     !
     ! double scm_to_double (SCM inexact)
     !
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: inexact
     real(c_double) :: d
   end function scm_to_double

   function scm_from_locale_stringn (str, len) result (string) bind (c)
     !
     ! SCM scm_from_locale_stringn (const char *str, size_t len)
     !
     use iso_c_binding
     implicit none
     character(kind=c_char) :: str(*)
     integer(c_size_t), intent(in), value :: len
     type(c_ptr) :: string
   end function scm_from_locale_stringn

   function scm_to_locale_stringbuf (str, buf, max_len) result (length) bind (c)
     !
     ! size_t scm_to_locale_stringbuf (SCM str, char *buf, size_t max_len)
     !
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: str
     character(kind=c_char) :: buf(*)
     integer(c_size_t), intent(in), value :: max_len
     integer(c_size_t) :: length
   end function scm_to_locale_stringbuf

   function scm_cons (car, cdr) result (pair) bind (c)
     !
     ! SCM scm_cons (SCM car, SCM cdr)
     !
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: car, cdr
     type(c_ptr) :: pair
   end function scm_cons

   function scm_car (pair) result (car) bind (c)
     !
     ! SCM scm_car (SCM pair)
     !
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: pair
     type(c_ptr) :: car
   end function scm_car

   function scm_cdr (pair) result (cdr) bind (c)
     !
     ! SCM scm_cdr (SCM pair)
     !
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: pair
     type(c_ptr) :: cdr
   end function scm_cdr

   function scm_string_to_symbol (string) result (symbol) bind (c)
     !
     ! SCM scm_string_to_symbol (SCM string)
     !
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: string
     type(c_ptr) :: symbol
   end function scm_string_to_symbol

   function scm_symbol_to_string (symbol) result (string) bind (c)
     !
     ! SCM scm_symbol_to_string (SCM symbol)
     !
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: symbol
     type(c_ptr) :: string
   end function scm_symbol_to_string

   function scm_lookup (symbol) result (variable) bind (c)
     !
     ! SCM scm_lookup (SCM name)
     !
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: symbol
     type(c_ptr) :: variable
   end function scm_lookup

   function scm_variable_ref (variable) result (value) bind (c)
     !
     ! SCM scm_variable_ref (SCM var)
     !
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: variable
     type(c_ptr) :: value
   end function scm_variable_ref
end interface

!
! Predicates:
!
public :: scm_is_symbol         ! SCM obj -> logical
public :: scm_is_string         ! SCM obj -> logical
public :: scm_is_number         ! SCM obj -> logical
public :: scm_is_pair           ! SCM obj -> logical
public :: scm_is_null           ! SCM obj -> logical

public :: scm_is_true           ! SCM obj -> logical
public :: scm_is_exact          ! SCM obj -> logical
public :: scm_is_inexact        ! SCM obj -> logical

!
! Accessors:
!
public :: scm_to_int            ! SCM int -> integer
public :: scm_to_double         ! SCM double -> double
public :: scm_to_stringbuf      ! SCM string -> (Maybe string, integer)

public :: scm_car               ! SCM pair -> SCM car
public :: scm_cdr               ! SCM pair -> SCM cdr

!
! Constructors:
!
public :: scm_from_int          ! integer -> SCM int
public :: scm_from_double       ! double -> SCM double
public :: scm_from_string       ! string -> SCM string

public :: scm_cons              ! SCM car -> SCM cdr -> SCM pair

contains

   function scm_is_true (object) result (yes)
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: object
     logical :: yes
     ! *** end of interface ***

     yes = guile_macro_scm_is_true (object) /= 0
   end function scm_is_true

   function scm_is_symbol (object) result (yes)
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: object
     logical :: yes
     ! *** end of interface ***

     yes = scm_is_true (scm_symbol_p (object))
   end function scm_is_symbol

   function scm_is_string (object) result (yes)
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: object
     logical :: yes
     ! *** end of interface ***

     yes = scm_is_true (scm_string_p (object))
   end function scm_is_string

   function scm_is_number (object) result (yes)
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: object
     logical :: yes
     ! *** end of interface ***

     yes = scm_is_true (scm_number_p (object))
   end function scm_is_number

   function scm_is_pair (object) result (yes)
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: object
     logical :: yes
     ! *** end of interface ***

     yes = scm_is_true (scm_pair_p (object))
   end function scm_is_pair

   function scm_is_null (object) result (yes)
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: object
     logical :: yes
     ! *** end of interface ***

     yes = scm_is_true (scm_null_p (object))
   end function scm_is_null

   function scm_is_exact (object) result (yes)
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: object
     logical :: yes
     ! *** end of interface ***

     yes = scm_is_true (scm_exact_p (object))
   end function scm_is_exact

   function scm_is_inexact (object) result (yes)
     use iso_c_binding
     implicit none
     type(c_ptr), intent(in), value :: object
     logical :: yes
     ! *** end of interface ***

     yes = scm_is_true (scm_inexact_p (object))
   end function scm_is_inexact

  function scm_from_string (for_string) result (scm_string)
    !
    ! Fortran string -> SCM string.
    !
    character(len=*), intent(in) :: for_string
    type(c_ptr) :: scm_string
    ! *** end of interface **

    integer(c_size_t) :: slen

    slen = len(for_string)
    scm_string = scm_from_locale_stringn (for_string, slen)
  end function scm_from_string

  subroutine scm_to_stringbuf (string, buf, length)
    !
    ! SCM string -> (Maybe (Fortran string), length)
    !
    ! The output string is only complete if length <= len(buf) on
    ! output.
    !
    type(c_ptr) :: string
    character(len=*), intent(out) :: buf
    integer, intent(out) :: length ! default kind, not an integer(c_size_t)
    ! *** end of interface **

    integer(c_size_t) :: max_len, size_t_length

    max_len = len(buf)
    size_t_length = scm_to_locale_stringbuf (string, buf, max_len)

    ! if ( size_t_length > max_len ) then
    !    print *, "scm_to_stringbuf: WARNING! String too long:", size_t_length, ">", max_len
    ! endif

    ! Convert to plain integer, FIXME: overflow?
    length = size_t_length

    ! clear trailing garbage, FIXME: should we?
    buf(length+1:max_len) = " "
  end subroutine scm_to_stringbuf

  function lookup (name) result (value)
    character(len=*), intent(in) :: name
    type(c_ptr) :: value
    ! *** end of interface **

    type(c_ptr) :: string, symbol, variable

    string = scm_from_string (name)
    symbol = scm_string_to_symbol (string)
    variable = scm_lookup (symbol)
    value = scm_variable_ref (variable)
  end function lookup

  recursive subroutine display (obj)
    implicit none
    type(c_ptr), intent(in) :: obj
    ! *** end of interface **

    character(len=128) :: buf
    integer :: slen

    if (scm_is_number (obj) ) then
       if (scm_is_exact (obj)) then
          write (*, '(I6)', advance='no') scm_to_int (obj)
       else
          write (*, '(F12.6)', advance='no') scm_to_double (obj)
       endif
    endif

    if (scm_is_symbol (obj)) then
       call scm_to_stringbuf (scm_symbol_to_string (obj), buf, slen)

       if ( slen > len(buf) ) then
          stop "display: ERROR: symbol too long!"
       endif

       write (*, "('''', A)", advance='no') buf(1:slen)
    endif

    if (scm_is_pair (obj)) then
       write (*, '("(")', advance='no')
       call display (scm_car (obj))
       write (*, '(" . ")', advance='no')
       call display (scm_cdr (obj))
       write (*, '(")")', advance='no')
    endif

    if (scm_is_null (obj)) then
       write (*, '("nil")', advance='no')
    endif
  end subroutine display

  function test (symbol, object) result (out) bind (c)
    implicit none
    type(c_ptr), intent(in), value :: symbol, object
    type(c_ptr) :: out
    ! *** end of interface ***

    character(len=16) :: buf
    integer :: slen

    call display (symbol)
    write (*, *) ! newline
    call display (object)
    write (*, *) ! newline

    call scm_to_stringbuf (scm_symbol_to_string (symbol), buf, slen)

    if ( slen > len(buf) ) then
       stop "test: ERROR: symbol too long!"
    end if

    out = scm_cons (lookup (buf(1:slen)), object)
  end function test

end module scm
