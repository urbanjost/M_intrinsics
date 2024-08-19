## ceiling

### **Name**

**ceiling**(3) - \[NUMERIC\] returns the least integer greater than or
equal to **a**.

### **Synopsis**
```fortran
    result = ceiling(a [,kind])
```
```fortran
     elemental integer(KIND) function ceiling(a,KIND)

      real(kind=**),intent(in)  :: a
      integer,intent(in),optional :: KIND
```
### **Characteristics**

 - **a** is of type _real_
 - if present **KIND** is a scalar integer constant expression that
   specifies the kind of the result.
 - the result is _integer_. It is default kind if **KIND** is not
   specified

### **Description**

  **ceiling**(3) returns the least integer greater than or equal to **a**.

  On the number line -n <-- 0 -> +n the value returned is always at or
  to the right of the input value.

  For example, ceil(0.5) is 1.0, and ceil(-0.5) is 0.0.

  The input value may be too large to store the result in an `integer`
  type. To avoid an overflow (which produces an undefined result), an
  application should perform a range check on the input value before
  using ceiling(3).

<!--
  If x is +0, -0, NaN, or infinite, x itself is returned.
-->
### **Options**

- **a**
  : A _real_ value to produce a ceiling for.

- **kind**
  : indicates the kind parameter of the result.

### **Result**

  The result will be the _integer_ value equal to **a** or the least
  integer greater than **a** if the input value is not equal to a
  whole number.

  If **a** is equal to a whole number, the returned value is **int(a)**.

  The result is undefined if it cannot be represented in the specified
  _integer_ type.

### **Examples**

Sample program:

```fortran
program demo_ceiling
implicit none
! just a convenient format for a list of integers
character(len=*),parameter :: gen='(1x,*(g0:,1x))'
real              :: x
real              :: y
real,parameter    :: arr(*)=[ &
   &  -2.7,  -2.5, -2.2, -2.0, -1.5, &
   &  -1.0,  -0.5,  0.0, +0.5, +1.0, &
   &  +1.5,  +2.0, +2.2, +2.5, +2.7  ]
integer           :: i
integer           :: ierr
character(len=80) :: message
   print *, 'Basic Usage'
   x = 63.29
   y = -63.59
   print gen, ceiling(x), ceiling(y)
   ! note the result was the next integer larger to the right

   print *, 'Whole Numbers' ! real values equal to whole numbers
   x = 63.0
   y = -63.0
   print gen, ceiling(x), ceiling(y)

   print *, 'Elemental' ! (so an array argument is allowed)
   print gen , ceiling(arr)

   print *, 'Advanced Usage' ! Dealing with large magnitude values
   print '(a)',[character(len=80):: &
   'Limits                                                           ',&
   'You only care about Limits if you are using values near or above ',&
   'the limits of the integer type you are using (see huge(3)).      ',&
   '',&
   'Surprised by some of the following results?                      ',&
   'What do real values clearly out of the range of integers return? ',&
   'What do values near the end of the range of integers return?     ',&
   'The standard only specifies what happens for representable values',&
   'in the range of integer values.                                  ',&
   '',&
   'It is common but not required that if the input is out of range  ',&
   'and positive the result is -huge(0) and -huge(0)-1 if negative.  ',&
   'Note you are out of range before you get to real(huge(0)).       ',&
   '' ]
   print gen , 'For reference: huge(0)=',huge(0),'-huge(0)-1=',-huge(0)-1

   x=huge(0)
   call displayx()

   x=2*x
   call displayx()

   x=-huge(0)-1
   call displayx()

   x=2*x
   call displayx()

   print gen , repeat('=',80)

contains

subroutine displayx()
use,intrinsic :: iso_fortran_env, only: int8,int16,int32,int64
   print gen , repeat('=',80)
   print gen , 'x=',x,' spacing=',spacing(x)
   print gen , ' ceiling(x):',ceiling(x)
   print gen , ' ceiling(x,kind=int64):',ceiling(x,kind=int64)
   print gen , ' ceiling_robust(x):',ceiling_robust(x,ierr,message)
   if(ierr.ne.0)then
      print gen, ierr,'=>',trim(message)
   endif
end subroutine displayx

elemental impure function ceiling_robust(x,ierr,message)
! return the least integer >= x
use,intrinsic :: iso_fortran_env, only: int8,int16,int32,int64
use,intrinsic :: iso_fortran_env, only: real32,real64,real128
real,intent(in)                       :: x
integer,intent(out),optional          :: ierr
character(len=*),intent(out),optional :: message
character(len=80)                     :: message_local
integer                               :: ceiling_robust
integer                               :: ierr_local
   ierr_local=0
   message_local=''
   ! allow -huge(0)-1 or not?
   if(spacing(x) > 128)then ! bounds checking
      if(x.ge.0)then
         write(message_local,*)'<ERROR>X=',x,' >=',anint(real(huge(0)))
         ierr_local=1
         ceiling_robust=huge(0)
      else
         ierr_local=2
         ceiling_robust=-huge(0)-1
         write(message_local,*)'<ERROR>X=',x,' <=',anint(real(-huge(0)-1))
      endif
   else
      ! used to use a computed goto to do this!
      ceiling_robust = int(x)
      if (x > 0.0) then
         if (real(ceiling_robust) < x)then
            ceiling_robust = ceiling_robust + 1
         endif
      endif
   endif
   if(present(ierr))then
      ierr=ierr_local
   elseif(ierr_local.ne.0)then
      stop message_local
   endif
   if(present(message))then
      message=message_local
   endif
end function ceiling_robust

end program demo_ceiling
```
Results:
```text
 >  Basic Usage
 >  64 -63
 >  Whole Numbers
 >  63 -63
 >  Elemental
 >  -2 -2 -2 -2 -1 -1 0 0 1 1 2 2 3 3 3
 > Limits
 >
 > Surprised by some of the following results?
 > What do real values clearly out of the range of integers return?
 > What do values near the end of the range of integers return?
 > The standard only specifies what happens for representable values
 > in the range of integer values.
 >
 > It is common but not required that if the input is out of range
 > and positive the result is -huge(0) and -huge(0)-1 if negative.
 > Note you are out of range before you get to real(huge(0)).
 >
 >  For reference: huge(0)= 2147483647 -huge(0)-1= -2147483648
 >  ======================================================================
 >  x= 0.214748365E+10  spacing= 256.000000
 >   ceiling(x): -2147483647
 >   ceiling(x,kind=int64): 2147483648
 >   ceiling_robust(x): 2147483647
 >  1 =>  <ERROR>X=   2.14748365E+09  >=   2.14748365E+09
 >  ======================================================================
 >  x= 0.429496730E+10  spacing= 512.000000
 >   ceiling(x): -2147483647
 >   ceiling(x,kind=int64): 4294967296
 >   ceiling_robust(x): 2147483647
 >  1 =>  <ERROR>X=   4.29496730E+09  >=   2.14748365E+09
 >  ======================================================================
 >  x= -0.214748365E+10  spacing= 256.000000
 >   ceiling(x): -2147483648
 >   ceiling(x,kind=int64): -2147483648
 >   ceiling_robust(x): -2147483648
 >  2 =>  <ERROR>X=  -2.14748365E+09  <=  -2.14748365E+09
 >  ======================================================================
 >  x= -0.429496730E+10  spacing= 512.000000
 >   ceiling(x): -2147483648
 >   ceiling(x,kind=int64): -4294967296
 >   ceiling_robust(x): -2147483648
 >  2 =>  <ERROR>X=  -4.29496730E+09  <=  -2.14748365E+09
 >  ======================================================================
```
### **Standard**

Fortran 95

### **See Also**

[**floor**(3)](#floor),
[**nint**(3)](#nint)

[**aint**(3)](#aint),
[**anint**(3)](#anint),
[**int**(3)](#int),
[**selected_int_kind**(3)](#selected_int_kind)

[**nearest**(3)](#nearest),
[**spacing**(3)](#spacing),
[**epsilon**(3)](#epsilon)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
