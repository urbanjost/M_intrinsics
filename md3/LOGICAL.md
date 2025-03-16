## logical

### **Name**

**logical**(3) - \[TYPE:CONVERSION\] Conversion between kinds of logical values

### **Synopsis**
```fortran
    result = logical(l [,kind])
```
```fortran
     elemental logical(kind=KIND) function logical(l,KIND)

      logical(kind=**),intent(in) :: l
      integer(kind=**),intent(in),optional :: KIND
```
### **Characteristics**

  - a kind designated as ** may be any supported kind for the type
  - **l** is of type logical
  - **KIND** shall be a scalar integer constant expression.
    If **KIND** is present, the kind type parameter of the result is
    that specified by the value of **KIND**; otherwise, the kind type
    parameter is that of default logical.

### **Description**

  **logical**(3) converts one kind of _logical_ variable to another.

### **Options**

- **l**
  : The _logical_ value to produce a copy of with kind **kind**

- **kind**
  : indicates the kind parameter of the result.
  If not present, the default kind is returned.

### **Result**

The return value is a _logical_ value equal to **l**, with a kind
corresponding to **kind**, or of the default logical kind if **kind**
is not given.

### **Examples**

Sample program:
```fortran
program demo_logical
use iso_fortran_env, only : logical_kinds
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use,intrinsic :: iso_fortran_env, only : real32, real64, real128
implicit none
character(len=*),parameter :: g='(*(g0))'
integer :: i, i1, i2
logical :: l1, l2
  !
  ! list kind values supported on this platform
  !
   do i =1, size(logical_kinds)
      write(*,'(*(g0))')'integer,parameter :: boolean', &
      & logical_kinds(i),'=', logical_kinds(i)
   enddo
  ! for performance and storage purposes you generally want
  ! to use the smallest storage size supported when using
  ! large arrays, but some existing routines may require
  ! the default kind. LOGICAL(3f) can change the kind of
  ! the variables.
  !
  ! But converting a logical to an integer is not done
  ! with LOGICAL(3f); but can be down with MERGE(3f).
  !
   l1=.true.
   l2=.false.
   i1=merge(0,1,l1)
   i2=merge(0,1,l2)
   write(*,g)'L1=',l1,' L2=',l2,' I1=',i1,' I2=',i2
  !
  ! show type and kind of default logicals
   call showme(.true.)
   call showme(l1)
  ! show logical() changing type and kind
   call showme(logical(l1))
  ! you may have to delete unsupported kinds from this example

  ! this is probably the default
   call showme(logical(l1,kind=4))
  ! note how showme shows different kinds are being passed to it
   call showme(logical(l1,kind=8))
   call showme(logical(l1,kind=2))
  ! this is probably the smallest storage size supported
  ! on this platform; but kind values are platform-specific
   call showme(logical(l1,kind=1))
contains
subroutine showme(val)
! @(#) showme(3f) - display type and kind of intrinsic value
class(*),intent(in) :: val
   select type(val)
      type is (integer(kind=int8))
        write(*,'("integer(kind=int8) ",i0)') val
      type is (integer(kind=int16))
         write(*,'("integer(kind=int16) ",i0)') val
      type is (integer(kind=int32))
         write(*,'("integer(kind=int32) ",i0)') val
      type is (integer(kind=int64))
         write(*,'("integer(kind=int64) ",i0)') val
      type is (real(kind=real32))
         write(*,'("real(kind=real32) ",1pg0)') val
      type is (real(kind=real64))
         write(*,'("real(kind=real64) ",1pg0)') val
      type is (real(kind=real128))
        write(*,'("real(kind=real128) ",1pg0)') val
      type is (logical(kind=1))
            write(*,'("logical(kind=1) ",l1,a,i0)') val, &
	    & 'storage=',storage_size(val)
      type is (logical(kind=2))
            write(*,'("logical(kind=2) ",l1,a,i0)') val, &
	    & 'storage=',storage_size(val)
      type is (logical(kind=4))
            write(*,'("logical(kind=4) ",l1,a,i0)') val, &
	    & 'storage=',storage_size(val)
      type is (logical(kind=8))
            write(*,'("logical(kind=8) ",l1,a,i0)') val, &
	    & 'storage=',storage_size(val)
      type is (character(len=*))
          write(*,'("character ",a)') trim(val)
      type is (complex)
                   write(*,'("","(",1pg0,",",1pg0,")")') val
      class default
      stop 'crud. showme() does not know about this type'
   end select
end subroutine showme
end program demo_logical
```
Results:
```text
 > integer,parameter :: boolean1=1
 > integer,parameter :: boolean2=2
 > integer,parameter :: boolean4=4
 > integer,parameter :: boolean8=8
 > integer,parameter :: boolean16=16
 > L1=T L2=F I1=0 I2=1
 > logical(kind=4) Tstorage=32
 > logical(kind=4) Tstorage=32
 > logical(kind=4) Tstorage=32
 > logical(kind=1) Tstorage=8
 > logical(kind=2) Tstorage=16
 > logical(kind=4) Tstorage=32
```
### **Standard**

Fortran 95 , related ISO_FORTRAN_ENV module - fortran 2009

### **See Also**
+ [**aimag**(3)](#aimag) - Imaginary part of complex number
+ [**cmplx**(3)](#cmplx) - Conversion to a complex type
+ [**dble**(3)](#dble) - Converstion to double precision real
+ [**int**(3)](#int) - Truncate towards zero and convert to integer
+ [**nint**(3)](#nint) - Nearest whole number
+ [**real**(3)](#real) - Convert to real type
+ [**out_of_range**(3)](#out_of_range) - Whether a numeric value can be
  converted safely to another type
+ [**transfer**(3)](#transfer) - Transfer bit patterns

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_

