## include

### **Name**
  include(7) - [PREPROCESS] blending source text

### **Synopsis**
  INCLUDE char-literal-constant

### **Description**
  Additional text may be incorporated into the source text of a program
  unit during processing. This is accomplished with the INCLUDE line,
  which typically has the form
```fortran
       INCLUDE "filename"
```
  An **include** line is not a Fortran statement. It is processed at
  compilation. The effect of the **include** line is as if the referenced
  source text physically replaced the **include** line prior to program
  processing. Included text may contain almost any source text, including
  additional **include** lines; such nested **include** lines are similarly
  replaced with the specified source text. The maximum depth of nesting
  of any nested **include** lines is processor dependent. Inclusion of the
  source text referenced by an **include** line shall not, at any level of
  nesting, result in inclusion of the same source text (ie. it cannot
  be recursive).

  The exceptions on what can be included in an **include** file are
  that the first included statement line cannot be a continuation line
  and the last included statement line cannot be continued.

  The interpretation of char-literal-constant is processor dependent.

  It is generally implemented as a filename containing text to be
  included, but could be interpreted as a URL or a system command that
  generates text or a database query, or a list of files, for example.
  That being said, all current implementations appear to at least treat
  it as a simple filename.

  Where the compiler searches for the filename is
  implementation-dependent. All current implementations appear to at least
  search for the file in the same directory as the file containing the
  INCLUDE statement if it is not a complete filepath specification. It
  is common but not required that other directories are searched as
  specified with the common -I switch found on most compiler commands.

  The char-literal-constant shall not have a kind type parameter value
  that is a named-constant. That is, it must be a quoted string. It
  cannot be something like
```fortran
        character(len=*),parameter :: filename='willnotwork.inc'
        include filename
```
  An INCLUDE line shall appear on a single source line where a statement
  may appear (many compilers support an extension allowing continuation
  lines to be supported); it must be the only nonblank text on the line
  other than an optional trailing comment (no statement label is allowed).
  So here are some bad ideas
```fortran
        INCLUDE "filename";I=10 ! NO: multiple statements on line
        100 INCLUDE 'filename'  ! NO: statement label not allowed
        ! continuation often works but is non-standard
        INCLUDE &
        & 'filename'
        INCLUDE 'file&
        &name'
```
   **Preprocessing**
  Note that an INCLUDE line is generally processed after any preprocessor
  so the INCLUDE file should not include preprocessor directives such as
  cpp(1) or fpp(1) directives. If that is required you probably need to
  use an equivalent preprocessor directive such as a cpp(1) "#include"
  directive instead of a Fortran INCLUDE.

   **Summary**
  So it is a de-facto standard that an INCLUDE at least supports a simple
  filename pointing to a file in the directory where the file containing
  the INCLUDE file resides or a full path name in single or double quotes.

  An INCLUDE statement was a common way to ensure a COMMONBLOCK was
  declared the same in multiple files (at least if every file with the
  INCLUDE was recompiled). It should generally be avoided and a MODULE
  should be used instead of a COMMONBLOCK in the vast majority of cases
  in new code.

   **Rules for Fixed and Free file format portability**
  If the code in your "include file" needs read by both old fixed-format
  files and free-format files it is not necessary to maintain two copies
  of the file.

  Observing the following rules allows included code to be used with
  either free or fixed source forms.

    *  Confine statement labels to character positions 1 to 5 and
       statements to character positions 7 to 72

    *  Treat blanks as being significant.

    *  Use only the exclamation mark (!) to indicate a comment, but do
       not start the comment in character position 6.

    *  For continued statements, place an ampersand (&) in both
       character position 73 of a continued line and character position 6
       of a continuation line.

### **Example**
  Sample program:

  In this example, the same code for the function subr is used to build
  a 32-bit and 64-bit version that are then merged into a generic name

  Given the file "subr.inc":
```fortran
   function subr(val)
   ! trivial function. What to note is
   ! all the kinds are specified via "WP"
   real(kind=wp) :: subr
   real(kind=wp),intent(in) :: val
      subr=sqrt(val*3.0_wp)
   end function subr
```
and we will throw in a few other files to do simple includes with as well.

declarations.inc
```fortran
integer :: i,j,k
```
somecode.inc
```fortran
write(*,*)'Hello World!'
```
somemorecode.inc
```fortran
subroutine another()
write(*,*)'Hello World!'
end subroutine another
```
```fortran
!program show_include
! define wp to be single precision
! and include file
module single
integer,parameter :: wp=kind(0.0)
contains
include "subr.inc"
end module single

module double
! define wp to be double precision
! and include file
integer,parameter :: wp=kind(0.0d0)
contains
include "subr.inc"
end module double

module merge
! so:     module single contains a 32-bit subr() procedure
! while:  module single contains a 64-bit subr() procedure
! make a generic subr() from the two versions
use single, only : subs=>subr
use double, only : subd=>subr
interface subr
   module procedure subs
   module procedure subd
end interface

end module merge

program show_include
use merge, only : subr
implicit none
include "declarations.inc"
   write(*,*)'Hello World!'
   write(*,*)subr(10.0)
   write(*,*)subr(20.0d0)
include "somecode.inc"
contains
include "somemorecode.inc"
end program show_include
```
