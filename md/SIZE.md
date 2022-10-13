## size

### **Name**

**size**(3) - \[ARRAY:INQUIRY\] Determine the size of an array

### **Synopsis**
```fortran
    result = size(array [,dim] [,kind])
```
```fortran
     integer(kind=KIND) function size(array,dim,kind)

      type(TYPE(kind=KIND),intent(in) :: array(..)
      integer(kind=**),intent(in),optional :: dim
      integer(kind=**),intent(in),optional :: KIND
```
### **Characteristics**

 - a kind designated as ** may be any supported kind value for the type

-  **array** may be of any type and associated kind.

   If **array** is a pointer it must be associated and allocatable arrays
   must be allocated.

### **Description**

  **size(3)** returns the total number of elements in an array, or
  if **dim** is specified returns the number of elements along that
  dimension.

  **size**(3) determines the extent of **array** along a specified
  dimension **dim**, or the total number of elements in **array** if
  **dim** is absent.

### **Options**

- **array**
  : the array to measure the number of elements of.

- **dim**
  : a value shall be
  in the range from 1 to n, where n equals the rank of **array**.

  If not present the total number of elements of the entire array
  are returned.

    If KIND is present, the KIND type parameter is that specified by
    the value of KIND; otherwise, the KIND type parameter is that of
    default integer type.

- **kind**
  : An _integer_ initialization expression indicating the kind
  parameter of the result.

  The **kind** must allow for the magnitude returned by **size** or
  results are undefined.

  If **kind** is absent, the return value is of default _integer_ kind.

### **Result**

  If **dim** is not present the total number of elements in the array
  are returned.

  If **dim** is present the number of elements along that dimension
  are returned.

### **Examples**

Sample program:

```fortran
program demo_size
implicit none
integer :: arr(0:2,-5:5)
   write(*,*)'SIZE of simple two-dimensional array'
   write(*,*)'SIZE(arr)       :total count of elements:',size(arr)
   write(*,*)'SIZE(arr,DIM=1) :number of rows         :',size(arr,dim=1)
   write(*,*)'SIZE(arr,DIM=2) :number of columns      :',size(arr,dim=2)

   ! pass the same array to a procedure that passes the value two
   ! different ways
   call interfaced(arr,arr)
contains

subroutine interfaced(arr1,arr2)
! notice the difference in the array specification
! for arr1 and arr2.
integer,intent(in) :: arr1(:,:)
integer,intent(in) :: arr2(2,*)
   !
   write(*,*)'interfaced assumed-shape array'
   write(*,*)'SIZE(arr1)        :',size(arr1)
   write(*,*)'SIZE(arr1,DIM=1)  :',size(arr1,dim=1)
   write(*,*)'SIZE(arr1,DIM=2)  :',size(arr1,dim=2)

!  write(*,*)'SIZE(arr2)        :',size(arr2)
   write(*,*)'SIZE(arr2,DIM=1)  :',size(arr2,dim=1)
!
! CANNOT DETERMINE SIZE OF ASSUMED SIZE ARRAY LAST DIMENSION
!  write(*,*)'SIZE(arr2,DIM=2)  :',size(arr2,dim=2)

end subroutine interfaced

end program demo_size
```
Results:
```text
    SIZE of simple two-dimensional array
    SIZE(arr)       :total count of elements:          33
    SIZE(arr,DIM=1) :number of rows         :           3
    SIZE(arr,DIM=2) :number of columns      :          11
    interfaced assumed-shape array
    SIZE(arr1)        :          33
    SIZE(arr1,DIM=1)  :           3
    SIZE(arr1,DIM=2)  :          11
    SIZE(arr2,DIM=1)  :           2
```
### **Standard**

Fortran 95 , with **kind** argument - Fortran 2003

### **See Also**

#### Array inquiry:

- [**size**(3)](#size) -  Determine the size of an array
- [**rank**(3)](#rank) -  Rank of a data object
- [**shape**(3)](#shape) -  Determine the shape of an array
- [**ubound**(3)](#ubound) -  Upper dimension bounds of an array
- [**lbound**(3)](#lbound) -  Lower dimension bounds of an array

#### State Inquiry:

- [**allocated**(3)](#allocated) -  Status of an allocatable entity
- [**is_contiguous**(3)](#is_contiguous) -  Test if object is contiguous

#### Kind Inquiry:

- [**kind**(3)](#kind) - Kind of an entity

#### Bit Inquiry:

- [**storage_size**(3)](#storage_size) - Storage size in bits
- [**bit_size**(3)](#bit_size) -  Bit size inquiry function
- [**btest**(3)](#btest) - Tests a bit of an _integer_ value.

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
<!--
ARRAY
    An array of any data type or an assumed-rank object.

   The corresponding actual argument must not be a scalar,
    disassociated pointer, or allocatable array that is not allocated. The
    actual argument can be an assumed-size array if DIM is present and
    has a value that is less than the rank of ARRAY.

DIM (optional)
    An INTEGER scalar. Its value must be in the range 1 <= DIM <=
    RANK(ARRAY). It must not be present if ARRAY is an
    assumed-rank object that is associated with a scalar.

    An INTEGER scalar. Its value must be specified by a constant expression. Fortran 2003 ends

    result is of type scalar integer.

Result value

The result equals the extent of ARRAY along dimension DIM; or, if DIM is not specified, it is the total number of array elements in ARRAY.
TS 29113 begins

    If ARRAY is an assumed-rank object that is associated with a scalar, the result is 1.
    If ARRAY is an assumed-rank object that is associated with an assumed-size array, and
        If DIM is present and equal to the rank of ARRAY, the result is -1.
        If DIM is not present, the result is a negative value that is equal to PRODUCT([(SIZE(ARRAY, I, KIND), I=1, RANK(ARRAY))]).
-->
