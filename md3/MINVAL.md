## minval

### **Name**

**minval**(3) - \[ARRAY:REDUCTION\] Minimum value of all the elements
of ARRAY along dimension DIM corresponding to true elements of MASK.

### **Synopsis**
forms
```fortran
    result = minval(array, [mask]) 
```
or
```fortran
    result = minval(array [,dim] [,mask])
```
```fortran
     type(TYPE(kind=**)) function minval(array, dim, mask)

      NUMERIC,intent(in) :: array(..)
      integer(kind=**),intent(in),optional :: dim
      logical(kind=**),intent(in),optional :: mask(..)
```
### **Characteristics**

 - **TYPE** may be real, integer, or character.
 - a kind designated as ** may be any supported kind for the type
 - **dim** is an integer scalar indicating a dimension of the array.
   It may not be an optional dummy argument.
 - **mask** is an array of type _logical_, and conformable with **array**.
 - the result is of the same type and kind as **array**.

### **Description**

  **minval**(3) determines the minimum value of the elements in an array
  or, if the **dim** argument is supplied, determines the minimum value
  in the subarrays indicated by stepping along the **dim**th dimension.

  Note that the result of 
```fortran
  MINVAL(ARRAY, MASK = MASK) 
```
  has a value equal to that of 
```fortran
  MINVAL (PACK (ARRAY, MASK)).
```
  and The result of 
```fortran
  MINVAL (ARRAY, DIM = DIM [, MASK = MASK])
```
  has a value equal to that of
```fortran
  MINVAL (ARRAY [, MASK = MASK])
```
  if ARRAY has rank one. Otherwise, the value of element
  (s1 , s2 , . . . , sDIM-1 , sDIM+1 , . . . , sn ) of the result is equal to
```fortran
  MINVAL (ARRAY (s1 , s2 , . . . , sDIM-1 , :, sDIM+1 , . . . , sn )
  [, MASK= MASK (s1 , s2 , . . . , sDIM-1 , :, sDIM+1 , . . . , sn ) ] ).
```
### **Options**

- **array**
  : array to search for minimum values. If the array has zero size,
  or all of the elements of **mask** are .false., then the result is
  **huge(array)** if **array** is numeric, or an array of strings of
  **char(len=len(array))** characters, with each character equal to
  CHAR (n - 1, KIND (ARRAY)), where n is the number of characters in
  the collating sequence for characters with the kind type parameter
  of **array**.

  If ARRAY is of type character, the result is the value that would be
  selected by application of intrinsic relational operators; that is,
  the collating sequence for characters with the kind type parameter of
  the arguments is applied.

- **dim**
  : Indicates which dimension to split the array into subarrays along.
  It has a value between one and the rank of **array**, inclusive.

- **mask**
  ; If **mask** is present, only the elements for which **mask** is _.true._
  are considered when searching for the minimal value.

### **Result**

If **dim** is absent, or if **array** has a rank of one, the result is a scalar.

If **dim** is present, the result is an array with a rank one less than the
rank of **array**, and a size corresponding to the size of **array** with the
**dim** dimension removed. In all cases, the result is of the same type and
kind as **array**.

### **Examples**

sample program:
```fortran
program demo_minval
implicit none
integer :: i
character(len=:),allocatable :: strs(:)
character(len=*),parameter :: g='(3x,*(g0,1x))'

integer,save :: ints(3,5)= reshape([&
       1,  -2,   3,   4,   5,  &
      10,  20, -30,  40,  50,  &
      11,  22,  33, -44,  55  &
],shape(ints),order=[2,1])

integer,save :: box(3,5,2)

   box(:,:,1)=ints
   box(:,:,2)=-ints

   write(*,*)'Given the array'
   write(*,'(1x,*(g4.4,1x))') &
   & (ints(i,:),new_line('a'),i=1,size(ints,dim=1))

   write(*,*)'What is the smallest element in the array?'
   write(*,g) minval(ints),'at <',minloc(ints),'>'

   write(*,*)'What is the smallest element in each column?'
   write(*,g) minval(ints,dim=1)

   write(*,*)'What is the smallest element in each row?'
   write(*,g) minval(ints,dim=2)

   ! notice the shape of the output has less columns
   ! than the input in this case
   write(*,*)'What is the smallest element in each column,'
   write(*,*)'considering only those elements that are'
   write(*,*)'greater than zero?'
   write(*,g) minval(ints, dim=1, mask = ints > 0)

   write(*,*)&
   & 'if everything is false a zero-sized array is NOT returned'
   write(*,*) minval(ints, dim=1, mask = .false.)
   write(*,*)'even for a zero-sized input'
   write(*,g) minval([integer ::], dim=1, mask = .false.)

   write(*,*)'a scalar answer for everything false is huge()'
   write(*,g) minval(ints, mask = .false.)
   write(*,g) minval([integer ::], mask = .false.)

   print *, 'if zero-size character array all dels if ASCII'
   strs=[character(len=5)::]
   strs=minval(strs)
   print g, ichar([(strs(i),i=1,len(strs))])

   write(*,*)'some calls with three dimensions'
   write(*,g) minval(box, mask = .true. )
   write(*,g) minval(box, dim=1, mask = .true. )

   write(*,g) minval(box, dim=2, mask = .true. )
   write(*,g) 'shape of answer is ', &
   & shape(minval(box, dim=2, mask = .true. ))

end program demo_minval
```
Result:
```text
 >  Given the array
 >     1   -2    3    4    5    
 >    10   20  -30   40   50    
 >    11   22   33  -44   55    
 > 
 >  What is the smallest element in the array?
 >    -44 at < 3 4 >
 >  What is the smallest element in each column?
 >    1 -2 -30 -44 5
 >  What is the smallest element in each row?
 >    -2 -30 -44
 >  What is the smallest element in each column,
 >  considering only those elements that are
 >  greater than zero?
 >    1 20 3 4 5
 >  if everything is false a zero-sized array is NOT returned
 >   2147483647  2147483647  2147483647  2147483647  2147483647
 >  even for a zero-sized input
 >    2147483647
 >  a scalar answer for everything false is huge()
 >    2147483647
 >    2147483647
 >  if zero-size character array all dels if ASCII
 > 
 >  some calls with three dimensions
 >    -55
 >    1 -2 -30 -44 5 -11 -22 -33 -40 -55
 >    -2 -30 -44 -5 -50 -55
 >    shape of answer is  3 2
```
### **Standard**

Fortran 95

### **See Also**

[**min**(3)](#min),
[**minloc**(3)](#minloc)
[**maxloc**(3)](#maxloc),
[**maxval**(3)](#maxval),
[**min**(3)](#min)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
