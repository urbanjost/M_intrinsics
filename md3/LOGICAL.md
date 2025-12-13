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

  For performance and storage purposes you generally want to use the
  smallest storage size supported when using large logical arrays, but
  some existing routines may require a specific kind. LOGICAL(3f) can
  change the kind of logical variables or expressions; but if converting
  is required frequently you might evaluate whether another kind is
  called for.

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
!
! The standard only requires one default logical kind to be supported
! of the same storage size as a default INTEGER and REAL but the
! following kind names are standard. The kind may not be 
! supported (in which case the value of the kind name will be a 
! negative integer value) and additional kinds may be available as well.
use,intrinsic :: iso_fortran_env, only : &
 & LOGICAL8, LOGICAL16, LOGICAL32, LOGICAL64
!
! C_BOOL is a kind compatible with C interfaces
use,intrinsic :: iso_c_binding,   only : C_BOOL
!
implicit none
character(len=*),parameter            :: all='(*(g0))'
integer                               :: i, i1, i2
! make T and F abbreviations for .TRUE. and .FALSE.
logical,parameter                     :: T=.true., F=.false.
logical                               :: l1, l2
! potentially save space and improve performance by using the
! smallest available kind
logical(kind=selected_logical_kind(1)) :: smallest_storage(10,20)
logical(kind=c_bool)                   :: boolean=.TRUE.
  !
  print all, 'list LOGICAL kind values available on this platform'
   do i =1, size(logical_kinds)
      write(*,all)'   integer,parameter :: boolean', &
      & logical_kinds(i),'=', logical_kinds(i)
   enddo

  print all, '   LOGICAL8  ==> KIND=',LOGICAL8
  print all, '   LOGICAL16 ==> KIND=',LOGICAL16
  print all, '   LOGICAL32 ==> KIND=',LOGICAL32
  print all, '   LOGICAL64 ==> KIND=',LOGICAL64
  print all, '   C_BOOL    ==> KIND=',C_BOOL

  print all, 'MERGE() is one method for transposing logical and integer'
  ! converting a logical to an integer is not done
  ! with LOGICAL(3f) and INT(3f) or promotion by assignment;
  ! but can be done with MERGE(3f) with scalars or arrays.
   i1=merge(0,1,T)
   i2=merge(0,1,F)
   write(*,all)'   T-->',i1,' F-->',I2
   l1=merge(T,F,i1.eq.0)
   l2=merge(T,F,i2.eq.0)
   write(*,all)'   0-->',l1,' 1-->',l2

  !
  ! Note the standard specifies the default INTEGER, REAL, and LOGICAL
  ! types have the same storage size, but compiler options often allow
  ! changing that. STORAGE_SIZE() can be used to confirm that.
  !
  print all, 'show kind and storage size of default logical'
   call showme(.true.)
   call showme(l1)
  ! A method to portably request the smallest storage size is
  !    logical(kind=selected_logical_kind(1) :: array(1000,1000)
  print all, 'storage size of smallest logical kind'
   call showme(logical(l1,kind=selected_logical_kind(1)))

  ! you may have to delete unsupported kinds from this example
  print all, 'different kinds are being passed because of LOGICAL() call'
  print all,'KIND values are platform-specific'
   call showme(logical(l1,kind=1))
   call showme(logical(l1,kind=2))
   call showme(logical(l1,kind=4))
   call showme(logical(l1,kind=8))
  print all,'kind=C_BOOL'
   call showme(logical(l1,kind=c_bool))
  print all,'SELECTED_LOGICAL_KIND() is more portable than KIND values'
  ! you might want to check the resulting kind
   call showme(logical(l1,kind=selected_logical_kind(1))) ! smallest
   call showme(logical(l1,kind=kind(.true.)))             ! default
   call showme(logical(l1,kind=selected_logical_kind(8)))
   call showme(logical(l1,kind=selected_logical_kind(16)))
   call showme(logical(l1,kind=selected_logical_kind(32)))
   call showme(logical(l1,kind=selected_logical_kind(64)))

contains
subroutine showme(val)
! @(#) showme(3f) - display type and kind of intrinsic value
! this is an example of how to accept any logical kind as a parameter,
! but this is often done with a generic procedure.
class(*),intent(in) :: val
   select type(val)
      type is (logical(kind=logical8))
            write(*,'("   logical(kind=1) ",l1,a,i0)') val, &
            & ' storage=',storage_size(val)
      type is (logical(kind=logical16))
            write(*,'("   logical(kind=2) ",l1,a,i0)') val, &
            & ' storage=',storage_size(val)
      type is (logical(kind=logical32))
            write(*,'("   logical(kind=4) ",l1,a,i0)') val, &
            & ' storage=',storage_size(val)
      type is (logical(kind=logical64))
            write(*,'("   logical(kind=8) ",l1,a,i0)') val, &
            & ' storage=',storage_size(val)
      class default
      stop 'crud. showme() does not know about this type'
   end select
end subroutine showme
end program demo_logical
```
Results:

```text
    > list LOGICAL kind values available on this platform
    >    integer,parameter :: boolean1=1
    >    integer,parameter :: boolean2=2
    >    integer,parameter :: boolean4=4
    >    integer,parameter :: boolean8=8
    >    integer,parameter :: boolean16=16
    >    LOGICAL8  ==> KIND=1
    >    LOGICAL16 ==> KIND=2
    >    LOGICAL32 ==> KIND=4
    >    LOGICAL64 ==> KIND=8
    >    C_BOOL    ==> KIND=1
    > MERGE() is one method for transposing logical and integer
    >    T-->0 F-->1
    >    0-->T 1-->F
    > show kind and storage size of default logical
    >    logical(kind=4) T storage=32
    >    logical(kind=4) T storage=32
    > storage size of smallest logical kind
    >    logical(kind=1) T storage=8
    > different kinds are being passed because of LOGICAL() call
    > KIND values are platform-specific
    >    logical(kind=1) T storage=8
    >    logical(kind=2) T storage=16
    >    logical(kind=4) T storage=32
    >    logical(kind=8) T storage=64
    > kind=C_BOOL
    >    logical(kind=1) T storage=8
    > SELECTED_LOGICAL_KIND() is more portable than KIND values
    >    logical(kind=1) T storage=8
    >    logical(kind=4) T storage=32
    >    logical(kind=1) T storage=8
    >    logical(kind=2) T storage=16
    >    logical(kind=4) T storage=32
    >    logical(kind=8) T storage=64
```
### **Standard**

Fortran 95 , related ISO_FORTRAN_ENV module - fortran 2009

### **See Also**
+ [**aimag**(3)](#aimag) - Imaginary part of complex number
+ [**cmplx**(3)](#cmplx) - Conversion to a complex type
+ [**dble**(3)](#dble) - Conversion to double precision real
+ [**int**(3)](#int) - Truncate towards zero and convert to integer
+ [**nint**(3)](#nint) - Nearest whole number
+ [**real**(3)](#real) - Convert to real type
+ [**out_of_range**(3)](#out_of_range) - Whether a numeric value can be
  converted safely to another type
+ [**transfer**(3)](#transfer) - Transfer bit patterns

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
