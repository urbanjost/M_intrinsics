## implicit

### **Name**

   **implicit**(7f) - [STATEMENT]  specify default type associated
   to a starting letter or disallow implicit typing

### **Synopsis**
```fortran
    implicit [NONE]|[declaration-type-spec (letter-spec-list)]
```
### **Description**
   Determine default mapping between the first letter of a data entity
   and a type. The default is the equivalent of the statement
```fortran
      implicit real(a-h,o-z),integer(i-n)
```
   Compiler switches often allow the default to be the commonly
   recommended
```fortran
      implicit none ! require all variables to have type statements
```
   This requires that the procedures be written using "strong typing";
   where every variable subsequently has to be defined in a type
   declaration statement.

   If implicit typing is turned off you do not need to know all the
   scoping rules for implicit typing, which by itself is a significant
   reason for turning it off.

   When a large number of variable names are used with strong typing
   a significant number of lines of code need added to declare the
   variables; but strong typing is still almost universally recommended
   where terseness is not critical (as is sometimes the case in
   interactive usage or quick prototyping).

   Every data entity has a type (**integer**, **real**, **character**,
   ...). If a type is not explicitly assigned to a variable or function
   it will (by default) be assigned one according to the following rule
   -- the type is **integer** if the name starts with the letters from
   I to N (the first two letters of the word "integer"); otherwise it
   defaults to **real**.

   The **implicit** statement allows the default rule to be changed or
   set to null.

   To turn off implicit typing enter one and only one **implicit**
   statement in the scoping unit

      implicit none ! Disable all implicit typing

   Each data entity will now require having a type declared explicitly
   (**integer**, **real**, **double**, **complex**, ...).

   The statement must appear after any USE statements and before any type
   declarations, including PARAMETER statements (which must know the
   rules to determine what type names are that have not been explicitly
   declared).

   In most new code implicit typing is turned off either with an
   "IMPLICIT NONE" or sometimes by a compiler switch. On the other hand,
   the majority of pre-fortran90 code depends on implicit defaults.

   Each prefix letter may have the type assigned to it declared only
   once in a unit.

   As previously stated, the default rule, expressed as an **implicit**
   statement is
```fortran
      implicit real(a-h,o-z),integer(i-n)
```
   To make the default for all names be a **doubleprecision** type one
   could enter
```fortran
      implicit doubleprecision (a-z)
```
   NOTE: The standard does not require constants to be affected, so a
   type suffix is required for most constants. That is, even if A is
   implicitly double-precision
```fortran
       A=123456789.01234 ! only the precision of a default REAL is retained
```
```fortran
       A=123456789.01234D0 ! precision of a double will be retained
```
   In another unit one might specify (multiple statements and compound
   statements are allowed, as illustrated):
```fortran
      implicit complex (c), doubleprecision (d)
      implicit integer (i)
      implicit logical (l)
      implicit real (r)
      implicit character(len=8) (a,b,e-h,j,k,m-q,s-z)
```
   There is no way to make some letters default to no type and others
   have a default. Either nothing has a default type or everything does.
   You can however make a default be a user-defined null type.

```fortran
      subroutine sub1()
      ! cannot do a "implicit none" on just some letters.
      ! and a type is required so
      ! implicit (a-h)  ! NOTE: NOT ALLOWED. TYPE IS REQUIRE0
      ! but you can make the default an user-defined type ...
      ! notice the (incidently empty) type is defined below
      ! the implicit statement
      implicit nil(a-h) ! or implicit type(nil) (a)
      !
      type nil
      end type nil
      type(nil) :: anull
      end subroutine sub1
```
  The default for an internal or module procedure is the mapping in the
  host scoping unit. That is, a single "IMPLICIT NONE" in the global
  top section of a module makes the default be "IMPLICIT NONE" in any
  contained procedure.

  Explicitly setting a variable type always overides the default so
  any data entity that is not explicitly declared by a type declaration
  statement, is not an intrinsic function, and is not accessed by use or
  host association is declared implicitly to be of the type (and type
  parameters) mapped from the first letter of its name, provided the
  mapping is not null. But anything accessed by a USE statement retains
  the type it had in the scoping unit in which it was declared.

  This means the mapping may be to a derived type that is inaccessible
  in the local scope if the derived type is accessible in the host
  scoping unit. That is, if you import the variable "FRED" of type
  "GOO" from a module; "FRED" is of type "GOO" even if type "GOO"
  is private in the module defining "FRED". That is, variable "FRED"
  retains the type "GOO" even if you cannot declare variables of type
  "GOO" in the current scoping unit.

  So the data entity is treated as if it were declared in an explicit
  type declaration in the outermost scoping unit in which it appears. An
  explicit type specification in a FUNCTION statement overrides
  an IMPLICIT statement for the name of the result variable of that
  function subprogram.

OPTIONS
    NONE         Turns off implicit typing. Recommended. It must
                 follow USE statements but be before any variable
                 declarations, including PARAMETER statements.  When used
                 there shall be no other IMPLICIT statements in the
                 scoping unit.

    TYPE() letter-spec  is
                (letter-or-range[,letter-or-range] [,letter-or-range] )
                 If the minus and second letter appear, the second
                 letter shall follow the first letter alphabetically.
                 A letter-spec consisting of two letter s separated by
                 a minus is equivalent to writing a list containing
                 all of the letters in alphabetical order in the
                 alphabetic sequence from the first letter through
                 the second letter. For example, A-C is equivalent
                 to A, B, C. The same letter shall not appear as a
                 single letter, or be included in a range of letters,
                 more than once in all of the IMPLICIT statements
                 in a scoping unit.

EXAMPLE
  The following are examples of the use of IMPLICIT statements:

```fortran
        module example_module
           implicit none
           ...
           interface
              function fun (i)    ! not all data entities need to
                 integer fun      ! be declared explicitly, so I
              end function fun    ! does not need declared
           end interface
        contains
           function jfun (j)      ! all data entities need to
              integer jfun, j     ! be declared explicitly.
              ...
           end function jfun
        end module example_module
```
```fortran
        subroutine sub
           implicit complex (c)
           CM = (3.0, 2.0)      ! CM is implicitly declared COMPLEX
           ...
        contains
           subroutine sub1
              IMPLICIT INTEGER (A, C)
              C = (0.0, 0.0) ! C is host associated and of
                             ! type complex
              Z = 1.0        ! Z is implicitly declared REAL
              A = 2          ! A is implicitly declared INTEGER
              CC = 1         ! CC is implicitly declared INTEGER
              ...
           end subroutine sub1
           subroutine sub2
              Z = 2.0         ! Z is implicitly declared REAL and
                              ! is different from the variable of
                              ! the same name in SUB1
              ...
           end subroutine sub2
           subroutine sub3
              USE EXAMPLE_MODULE ! Accesses integer function FUN
                                  ! by use association
              Q = FUN (K)         ! Q is implicitly declared REAL and
              ...                 ! K is implicitly declared INTEGER
           end subroutine sub3
        end subroutine sub
```

   The following is an example of a mapping to a derived type that is
   inaccessible in the local scope:

```fortran
              program main
                implicit type(blob) (a)
                type blob
                  integer :: i
                end type blob
                type(blob) :: b
                call steve
              contains
                subroutine steve
                  integer :: blob
                  !..
                  aa = b
                  !..
                end subroutine steve
              end program main
```
   In the subroutine **steve()**, it is not possible to explicitly declare
   a variable to be of type **blob** because **blob** has been given a
   different meaning, but implicit mapping for the letter A still maps
   to type **blob**, so AA is of type **blob**.
```fortran
   program demo_implicit
   ! everything accessed via USE already has a type and comes
   ! before an implicit statement; but implicit rules are not
   ! inherited from modules
   use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT
   !
   ! the implicit statement must come before other declarations
   ! in new code using this turns on strong typing (that is,every
   ! variable has to have its type declared in a statement). This
   ! is generally highly recommended for new code.
   implicit none
   ! it is still a convention used by many programmers to reserve
   ! starting letters of I to N for integers.
   integer    :: i, j, k
   type(real) :: x,y,z
   intrinsic sin,cos ! intrinsic types are already specified
   integer,external :: zzz ! but external functions need declared
                           ! if they do not have an interface
   call sub1()
   call sub2()
   contains
   subroutine sub1()
   ! the implicit none above became the default for contained
   ! procedures so no reason to repeat it. So only required once
   ! in main procedure or once in top of a module to change the
   ! default of all procedures defined after a CONTAINS statement
   integer :: i=10,j=20
      write(*,*)'I=',i,'J=',j
   end subroutine sub1
   subroutine sub2()
   ! a contained subroutine can override the default created in the
   ! containing scope though
   implicit complex(a-z)
      A=(10,20)
      write(*,*)'A=',a
   end subroutine sub2
   end
   integer function zzz()
       zzz=1234
   end function zzz
   !end program demo_implicit
```
Results:
```text
 >  I=          10 J=          20
 >  A=             (10.0000000,20.0000000)
```
_Fortran intrinsic descriptions (license: MIT) \@urbanjost_
