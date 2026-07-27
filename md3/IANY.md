## iany

### **Name**

**iany**(3) - \[BIT:LOGICAL\] Bitwise OR of array elements

### **Synopsis**
```fortran
    result = iany(array [,mask]) | iany(array ,dim [,mask])
```
```fortran
     integer(kind=KIND) function iany(array,dim,mask)

      integer(kind=KIND),intent(in)        :: array(..)
      integer(kind=**),intent(in),optional :: dim
      logical(kind=**),intent(in),optional :: mask(..)
```
### **Characteristics**

 - **array** is an _integer_ array
 - **dim** may be of any _integer_ kind.
 - **mask** is a _logical_ array that conforms to **array**
 - The result will be of the same type and kind
   as **array**. It is scalar if **dim** does not appear or is 1.
   Otherwise, it is the shape and rank of array reduced by the
   dimension **dim**.

  note a kind designated as ** may be any supported kind for the type

### **Description**

  **iany**(3) reduces with bitwise **OR** (inclusive **OR**) the
  elements of **array** along dimension **dim** if the corresponding
  element in **mask** is _.true._.

### **Options**

- **array**
  : an array of elements to selectively **OR** based on the mask.

- **dim**
  : a value in the range from **1 to n**, where **n** equals the rank
  of **array**.

- **mask**
  : a _logical_ scalar; or an array of the same shape as **array**.

### **Result**

  The result is of the same type as **array**.

  If **dim** is absent, a scalar with the bitwise _or_ of all elements in
  **array** is returned. Otherwise, an array of rank **n-1**, where **n**
  equals the rank of **array**, and a shape similar to that of **array**
  with dimension **dim** dropped is returned.

### **Examples**

Sample program:

```fortran
program demo_iany
use, intrinsic :: iso_fortran_env, only : compiler_version
   use, intrinsic :: iso_fortran_env, only : integer_kinds, &
   & int8, int16, int32, int64
   implicit none
   logical,parameter :: T=.true., F=.false.
   integer(kind=int8) :: a(3)
   integer(kind=int8) :: answer

   print '(2a)', 'This file was compiled by ', compiler_version()

   ! set some values to exercise with
   a(1) = int(b'00100100',int8)
   a(2) = int(b'01101010',int8)
   a(3) = int(b'10101010',int8)
   answer=int(b'11101110',int8)
   ! if any bit on in any element of A it should be on in answer

   ! basic call
   print '("A=")'
   print '("  ",1x,b8.8)', a
   print '("IANY(A)=",1x,b8.8,/)', iany(a)
   print '("is it the expected value? ",1x,l1,/)', iany(a)==answer

   ! select values with a mask
   write(*,*)'IANY(A) with a mask'
   write(*,*)'these values should be equivalent'
   print '(1x,b8.8)', iany(a,mask=[T,F,T])  
   print '(1x,b8.8)', iany(a,[T,F,T])  
   print '(1x,b8.8)', iany(a,dim=1,mask=[T,F,T])

   print *
   print '("the answer should match",1x,b8.8)', iany([a(1),a(3)])
   write(*,*)'does it?'
   write(*,*)iany(a,[T,F,T]) == iany([a(1),a(3)])
   write(*,*)iany(a,[T,F,T]) == int(b'10101110',int8)

end program demo_iany
```
Results:
```text
 > This file was compiled by GCC version 16.1.1 20260613
 > A=
 >    00100100
 >    01101010
 >    10101010
 > IANY(A)= 11101110
 > 
 > is it the expected value?  T
 > 
 >  IANY(A) with a mask
 >  these values should be equivalent
 >  10101110
 >  10101110
 >  10101110
 > 
 > the answer should match 10101110
 >  does it?
 >  T
 >  T
```
### **Standard**

Fortran 2008

### **See Also**

[**iparity**(3)](#iparity),
[**iall**(3)](#iall),
[**ior**(3)](#ior)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
