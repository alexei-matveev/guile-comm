module scm

use iso_c_binding
implicit none
private

!
! Short structs  with size matching that  of some integer  type may be
! passed/returned  in  registers. That  depends  on  the platform  ABI
! convention.   BIND(C)  on  the  type(scm_t)  and  relevant  function
! interfaces requests the Fortran compiler to emulate the behaviour of
! (some) C compiler.
!
! http://gcc.gnu.org/onlinedocs/gcc/Code-Gen-Options.html
! www.x86-64.org/documentation/abi.pdf
!
! FIXME: Intel  compiler versions 11.1 and  12.1 break ABI  by using a
! custom convention of returning a struct. In this case one might want
! to replace all instances of type(scm_t) by integer(c_intptr_t).
!
type, public, bind(c) :: scm_t
   private
   integer(c_intptr_t) :: ptr
end type scm_t
!define SCM_T integer(c_intptr_t)
!define SCM_T type(scm_t)
!define SCM_T type(c_ptr)

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
     import
     implicit none
     type(scm_t), intent(in), value :: object
     type(scm_t) :: yes
   end function scm_symbol_p

   function scm_string_p (object) result (yes) bind (c)
     !
     ! SCM scm_string_p (SCM obj)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: object
     type(scm_t) :: yes
   end function scm_string_p

   function scm_number_p (object) result (yes) bind (c)
     !
     ! SCM scm_number_p (SCM obj)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: object
     type(scm_t) :: yes
   end function scm_number_p

   function scm_pair_p (object) result (yes) bind (c)
     !
     ! SCM scm_pair_p (SCM obj)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: object
     type(scm_t) :: yes
   end function scm_pair_p

   function scm_null_p (object) result (yes) bind (c)
     !
     ! SCM scm_null_p (SCM x)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: object
     type(scm_t) :: yes
   end function scm_null_p

   !
   ! These may be usefull to tell appart the instances of of number?:
   !
   function scm_exact_p (object) result (yes) bind (c)
     !
     ! SCM scm_exact_p (SCM x)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: object
     type(scm_t) :: yes
   end function scm_exact_p

   function scm_inexact_p (object) result (yes) bind (c)
     !
     ! SCM scm_inexact_p (SCM x)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: object
     type(scm_t) :: yes
   end function scm_inexact_p

   function scm_list_p (obj) result (yes) bind (c)
     !
     ! SCM scm_list_p (SCM obj)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: obj
     type(scm_t) :: yes
   end function scm_list_p

   !
   ! Type constructors/accessors, integers, doubles, strings,
   ! logicals, lists:
   !

   !
   ! FIXME: Guile API exports a macro for some of these, we added a
   ! function, see guile-api.c:
   !
   function guile_macro_scm_is_true (object) result (yes) bind (c, name="scm_is_true")
     !
     ! int scm_is_true (SCM obj)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: object
     integer(c_int) :: yes
   end function guile_macro_scm_is_true

   function scm_from_int (i) result (exact) bind (c, name="guile_macro_scm_from_int")
     !
     ! SCM scm_from_int (int i)
     !
     import
     implicit none
     integer(c_int), intent(in), value :: i
     type(scm_t) :: exact
   end function scm_from_int

   !
   ! FIXME: Guile API exports a macro for this, see guile-api.c:
   !
   function scm_to_int (exact) result (i) bind (c, name="guile_macro_scm_to_int")
     !
     ! int scm_from_int (SCM exact)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: exact
     integer(c_int) :: i
   end function scm_to_int

   function scm_from_double (d) result (inexact) bind (c)
     !
     ! SCM scm_from_double (double d)
     !
     import
     implicit none
     real(c_double), intent(in), value :: d
     type(scm_t) :: inexact
   end function scm_from_double

   function scm_to_double (inexact) result (d) bind (c)
     !
     ! double scm_to_double (SCM inexact)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: inexact
     real(c_double) :: d
   end function scm_to_double

   function scm_from_locale_stringn (str, len) result (string) bind (c)
     !
     ! SCM scm_from_locale_stringn (const char *str, size_t len)
     !
     import
     implicit none
     character(kind=c_char) :: str(*)
     integer(c_size_t), intent(in), value :: len
     type(scm_t) :: string
   end function scm_from_locale_stringn

   function scm_to_locale_stringbuf (str, buf, max_len) result (length) bind (c)
     !
     ! size_t scm_to_locale_stringbuf (SCM str, char *buf, size_t max_len)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: str
     character(kind=c_char) :: buf(*)
     integer(c_size_t), intent(in), value :: max_len
     integer(c_size_t) :: length
   end function scm_to_locale_stringbuf

   function scm_cons (car, cdr) result (pair) bind (c)
     !
     ! SCM scm_cons (SCM car, SCM cdr)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: car, cdr
     type(scm_t) :: pair
   end function scm_cons

   function scm_eol () result (empty) bind (c)
     !
     ! SCM SCM_EOL
     !
     import
     implicit none
     type(scm_t) :: empty
   end function scm_eol

   function scm_car (pair) result (car) bind (c)
     !
     ! SCM scm_car (SCM pair)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: pair
     type(scm_t) :: car
   end function scm_car

   function scm_cdr (pair) result (cdr) bind (c)
     !
     ! SCM scm_cdr (SCM pair)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: pair
     type(scm_t) :: cdr
   end function scm_cdr

   function scm_length (list) result (length) bind (c)
     !
     ! SCM scm_length (SCM list)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: list
     type(scm_t) :: length
   end function scm_length

   function scm_string_to_symbol (string) result (symbol) bind (c)
     !
     ! SCM scm_string_to_symbol (SCM string)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: string
     type(scm_t) :: symbol
   end function scm_string_to_symbol

   function scm_symbol_to_string (symbol) result (string) bind (c)
     !
     ! SCM scm_symbol_to_string (SCM symbol)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: symbol
     type(scm_t) :: string
   end function scm_symbol_to_string

   function scm_defined_p (symbol, scope) result (yes) bind (c)
     !
     ! SCM scm_defined_p (SCM symbol, SCM scope)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: symbol, scope
     type(scm_t) :: yes
   end function scm_defined_p

   function scm_lookup (symbol) result (variable) bind (c)
     !
     ! SCM scm_lookup (SCM name)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: symbol
     type(scm_t) :: variable
   end function scm_lookup

   function scm_variable_bound_p (var) result (yes) bind (c)
     !
     ! SCM scm_variable_bound_p (SCM var)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: var
     type(scm_t) :: yes
   end function scm_variable_bound_p

   function scm_variable_ref (variable) result (value) bind (c)
     !
     ! SCM scm_variable_ref (SCM var)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: variable
     type(scm_t) :: value
   end function scm_variable_ref

   function scm_undefined () result (undef) bind (c)
     !
     ! SCM SCM_UNDEFINED
     !
     import
     implicit none
     type(scm_t) :: undef
   end function scm_undefined

   function scm_define (name, val) result (var) bind (c)
     !
     ! SCM scm_define (SCM name, SCM val)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: name, val
     type(scm_t) :: var
   end function scm_define

   function scm_c_define_gsubr (name, req, opt, rst, fcn) result (proc) bind (c)
     !
     ! SCM scm_c_define_gsubr (const char *name, int req, int opt, int rst, fcn)
     !
     import
     implicit none
     character(kind=c_char) :: name(*) ! null-terminated, of course
     integer(c_int), intent(in), value :: req, opt, rst
     type(c_funptr), intent(in), value :: fcn
     type(scm_t) :: proc
   end function scm_c_define_gsubr
end interface

interface scm_call
   function scm_call_0 (proc) result (res) bind (c)
     !
     ! SCM scm_call_0 (SCM proc)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: proc
     type(scm_t) :: res
   end function scm_call_0

   function scm_call_1 (proc, arg1) result (res) bind (c)
     !
     ! SCM scm_call_1 (SCM proc, SCM arg1)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: proc, arg1
     type(scm_t) :: res
   end function scm_call_1

   function scm_call_2 (proc, arg1, arg2) result (res) bind (c)
     !
     ! SCM scm_call_2 (SCM proc, SCM arg1, SCM arg2)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: proc, arg1, arg2
     type(scm_t) :: res
   end function scm_call_2

   function scm_call_3 (proc, arg1, arg2, arg3) result (res) bind (c)
     !
     ! SCM scm_call_3 (SCM proc, SCM arg1, SCM arg2, SCM arg3)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: proc, arg1, arg2, arg3
     type(scm_t) :: res
   end function scm_call_3

   function scm_call_4 (proc, arg1, arg2, arg3, arg4) result (res) bind (c)
     !
     ! SCM scm_call_4 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: proc, arg1, arg2, arg3, arg4
     type(scm_t) :: res
   end function scm_call_4

   function scm_call_5 (proc, arg1, arg2, arg3, arg4, arg5) result (res) bind (c)
     !
     ! SCM scm_call_4 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5)
     !
     import
     implicit none
     type(scm_t), intent(in), value :: proc, arg1, arg2, arg3, arg4, arg5
     type(scm_t) :: res
   end function scm_call_5

   !
   ! FIXME: extend up to 9 if necessary.
   !
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

public :: scm_list_p            ! SCM obj -> SCM logical

!
! Accessors:
!
public :: scm_to_int            ! SCM int -> integer
public :: scm_to_double         ! SCM double -> double
public :: scm_to_stringbuf      ! SCM string -> (Maybe string, integer)

public :: scm_car               ! SCM pair -> SCM car
public :: scm_cdr               ! SCM pair -> SCM cdr
public :: scm_length            ! SCM list -> SCM int

!
! Constructors:
!
public :: scm_from_int          ! integer -> SCM int
public :: scm_from_double       ! double -> SCM double
public :: scm_from_string       ! string -> SCM string
public :: scm_string_to_symbol  ! SCM string -> SCM symbol

public :: scm_cons              ! SCM car -> SCM cdr -> SCM pair
public :: scm_eol               ! () -> SCM empty

!
! Procedures:
!
public :: scm_call              ! SCM proc -> ... -> SCM res

!
! Setting/quering environment:
!
public :: scm_define            ! SCM symbol -> SCM value -> SCM variable
public :: scm_defined_p         ! SCM symbol -> SCM scope -> SCM bool
public :: scm_lookup            ! SCM symbol -> SCM variable
public :: scm_variable_bound_p  ! SCM variable -> SCM bool
public :: scm_variable_ref      ! SCM variable -> SCM value
public :: scm_undefined         ! () -> SCM_UNDEFINED
public :: scm_define_gsubr      ! (character, integer, integer, integer, c_funptr) -> SCM proc

contains

   function scm_is_true (object) result (yes)
     implicit none
     type(scm_t), intent(in), value :: object
     logical :: yes
     ! *** end of interface ***

     yes = guile_macro_scm_is_true (object) /= 0
   end function scm_is_true

   function scm_is_symbol (object) result (yes)
     implicit none
     type(scm_t), intent(in), value :: object
     logical :: yes
     ! *** end of interface ***

     yes = scm_is_true (scm_symbol_p (object))
   end function scm_is_symbol

   function scm_is_string (object) result (yes)
     implicit none
     type(scm_t), intent(in), value :: object
     logical :: yes
     ! *** end of interface ***

     yes = scm_is_true (scm_string_p (object))
   end function scm_is_string

   function scm_is_number (object) result (yes)
     implicit none
     type(scm_t), intent(in), value :: object
     logical :: yes
     ! *** end of interface ***

     yes = scm_is_true (scm_number_p (object))
   end function scm_is_number

   function scm_is_pair (object) result (yes)
     implicit none
     type(scm_t), intent(in), value :: object
     logical :: yes
     ! *** end of interface ***

     yes = scm_is_true (scm_pair_p (object))
   end function scm_is_pair

   function scm_is_null (object) result (yes)
     implicit none
     type(scm_t), intent(in), value :: object
     logical :: yes
     ! *** end of interface ***

     yes = scm_is_true (scm_null_p (object))
   end function scm_is_null

   function scm_is_exact (object) result (yes)
     implicit none
     type(scm_t), intent(in), value :: object
     logical :: yes
     ! *** end of interface ***

     yes = scm_is_true (scm_exact_p (object))
   end function scm_is_exact

   function scm_is_inexact (object) result (yes)
     implicit none
     type(scm_t), intent(in), value :: object
     logical :: yes
     ! *** end of interface ***

     yes = scm_is_true (scm_inexact_p (object))
   end function scm_is_inexact

  function scm_from_string (for_string) result (scm_string)
    !
    ! Fortran string -> SCM string.
    !
    character(len=*), intent(in) :: for_string
    type(scm_t) :: scm_string
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
    type(scm_t) :: string
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

   function scm_define_gsubr (name, req, opt, rst, fcn) result (proc)
     !
     ! Fortranish wrapper (appends C_NULL_CHAR to name) for
     !
     ! SCM scm_c_define_gsubr (const char *name, int req, int opt, int rst, fcn)
     !
     implicit none
     character(len=*) :: name
     integer(c_int), intent(in) :: req, opt, rst
     type(c_funptr), intent(in) :: fcn
     type(scm_t) :: proc
     ! *** end of interface ***

     proc = scm_c_define_gsubr (name // C_NULL_CHAR, req, opt, rst, fcn)
   end function scm_define_gsubr

  function define (key, val) result (var)
    implicit none
    character(len=*), intent(in) :: key
    type(scm_t), intent(in), value :: val
    type(scm_t) :: var
    ! *** end of interface ***

    var = scm_define (scm_string_to_symbol (scm_from_string (key)), val)
  end function define

  function lookup (key) result (value)
    character(len=*), intent(in) :: key
    type(scm_t) :: value
    ! *** end of interface **

    type(scm_t) :: string, symbol, variable

    string = scm_from_string (key)
    symbol = scm_string_to_symbol (string)
    variable = scm_lookup (symbol)
    value = scm_variable_ref (variable)
  end function lookup

  recursive subroutine display (obj)
    implicit none
    type(scm_t), intent(in) :: obj
    ! *** end of interface **

    character(len=128) :: buf
    integer :: slen

    if (scm_is_number (obj) ) then

       if (scm_is_exact (obj)) then
          write (*, '(I6)', advance='no') scm_to_int (obj)
       else
          write (*, '(F12.6)', advance='no') scm_to_double (obj)
       endif

    else if (scm_is_string (obj)) then

       call scm_to_stringbuf (obj, buf, slen)

       if ( slen > len(buf) ) then
          stop "display: ERROR: string too long!"
       endif

       write (*, "('""', A, '""')", advance='no') buf(1:slen)

    else if (scm_is_symbol (obj)) then

       call scm_to_stringbuf (scm_symbol_to_string (obj), buf, slen)

       if ( slen > len(buf) ) then
          stop "display: ERROR: symbol too long!"
       endif

       write (*, "('''', A)", advance='no') buf(1:slen)

    else if (scm_is_null (obj)) then

       write (*, '("()")', advance='no')

    else if (scm_is_pair (obj)) then

       write (*, '("(")', advance='no')
       call display (scm_car (obj))
       write (*, '(" . ")', advance='no')
       call display (scm_cdr (obj))
       write (*, '(")")', advance='no')

    else
       write (*, '("???")', advance='no')
    endif
  end subroutine display

  function test (symbol, object) result (out) bind (c)
    !
    ! FIXME:  Note that  even though  this funciton  is  BIND(C), when
    ! compiled  by  Intel  the   resulting  interface  is  NOT  binary
    ! compatible to
    !
    !     intptr_t test (intptr_t symbol, intptr_t object)
    !
    ! so that  calling it from C/Guile  will fail. The  call will even
    ! fail  from Intel,  as  the caller  assumes  the correct  (quoted
    ! above) type signature.
    !
    ! In  effect,  it  appears  that extending  Guile  interpreter  by
    ! fortran funcitons returning SCM  objects is impossible, since it
    ! would not work at least  with one compiler. It works though with
    ! Gfortran.
    !
    implicit none
    type(scm_t), intent(in), value :: symbol, object
    type(scm_t) :: out
    ! *** end of interface ***

    character(len=16) :: buf
    integer :: slen
    type(scm_t) :: var

    call display (symbol)
    write (*, *) ! newline
    call display (object)
    write (*, *) ! newline

    call scm_to_stringbuf (scm_symbol_to_string (symbol), buf, slen)

    if ( slen > len(buf) ) then
       stop "test: ERROR: symbol too long!"
    end if

    out = scm_cons (scm_eol(), scm_cons (lookup (buf(1:slen)), object))
    call display (out)
    write (*, *)

    var = define ("*string*", scm_from_string ("hello, world!"))
    var = define ("*double*", scm_from_double (0.3d0))
    var = define ("*integer*", scm_from_int (7))
  end function test

end module scm
