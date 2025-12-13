## selected_int_kind

### **Name**

**selected_int_kind**(3) - \[KIND\] Choose integer kind

### **Synopsis**
```fortran
    result = selected_int_kind(r)
```
```fortran
    integer function selected_int_kind(r)

     integer(kind=KIND),intent(in) :: r
```
### **Characteristics**

 - **r** is an _integer_ scalar.
 - the result is an default integer scalar.

### **Description**

  **selected_int_kind**(3) return the kind value of the smallest
  integer type that can represent all values ranging from **-10\*\*r**
  (exclusive) to **10\*\*r** (exclusive). If there is no integer kind
  that accommodates this range, selected_int_kind returns **-1**.

### **Options**

- **r**
  : The value specifies the required range of powers of ten that need
    supported by the kind type being returned.

### **Result**

  The result has a value equal to the value of the kind type parameter
  of an integer type that represents all values in the requested range.

  if no such kind type parameter is available on the processor, the
  result is -1.

  If more than one kind type parameter meets the criterion, the value
  returned is the one with the smallest decimal exponent range, unless
  there are several such values, in which case the smallest of these
  kind values is returned.

### **Examples**

Sample program:

```fortran
program demo_selected_int_kind
use iso_fortran_env, only: output_unit, INTEGER_KINDS
use,intrinsic :: iso_fortran_env, only : compiler_version
implicit none
character(len=*),parameter :: all='(*(g0))'
integer,parameter :: k5 = selected_int_kind(5)
integer,parameter :: k15 = selected_int_kind(15)
integer           :: i, ii
integer(kind=k5)  :: i5
integer(kind=k15) :: i15
   ! write a program that can print attributes about each available kind
   print all,'program kinds'
   print all, &
      '! This file was written by ', compiler_version()
   do i=1,size(INTEGER_KINDS)
      ii=integer_kinds(i)
      print all,'integer,parameter :: i',ii,'=',ii
   enddo
   do i=1,size(INTEGER_KINDS)
      ii=integer_kinds(i) 
      print all, &
      'write(*,*)"huge(0_i', &
      ii, &
      ')=",huge(0_i', &
      ii, &
      ')'
   enddo
   print all,'end program kinds'

   print *
   print *, huge(i5), huge(i15)
   ! the following inequalities are always true
   print *, huge(i5) >= 10_k5**5-1
   print *, huge(i15) >= 10_k15**15-1

end program demo_selected_int_kind
```
Results:
```text
 > program kinds
 > ! This file was written by GCC version 13.1.0
 > integer,parameter :: i1=1
 > integer,parameter :: i2=2
 > integer,parameter :: i4=4
 > integer,parameter :: i8=8
 > integer,parameter :: i16=16
 > write(*,*)"huge(0_i1)=",huge(0_i1)
 > write(*,*)"huge(0_i2)=",huge(0_i2)
 > write(*,*)"huge(0_i4)=",huge(0_i4)
 > write(*,*)"huge(0_i8)=",huge(0_i8)
 > write(*,*)"huge(0_i16)=",huge(0_i16)
 > end program kinds
 > 
 >   2147483647  9223372036854775807
 >  T
 >  T
```
### **Standard**

Fortran 95

### **See Also**

[**aint**(3)](#aint),
[**anint**(3)](#anint),
[**int**(3)](#int),
[**nint**(3)](#nint),
[**ceiling**(3)](#ceiling),
[**floor**(3)](#floor)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
