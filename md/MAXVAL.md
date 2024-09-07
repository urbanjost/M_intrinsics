## maxval

### **Name**

**maxval**(3) - \[ARRAY:REDUCTION\] Determines the maximum value in an array or row

### **Synopsis**
```fortran
    result = maxval(array [,mask]) | maxval(array [,dim] [,mask])
```
```fortran
     NUMERIC function maxval(array ,dim, mask)

      NUMERIC,intent(in) :: array(..)
      integer(kind=**),intent(in),optional :: dim
      logical(kind=**),intent(in),optional :: mask(..)
```
### **Characteristics**

 - a kind designated as ** may be any supported kind for the type
 - **NUMERIC** designates any numeric type and kind.

### **Description**

  **maxval**(3) determines the maximum value of the elements in an
  array value, or, if the **dim** argument is supplied, determines the
  maximum value along each row of the array in the **dim** direction. If
  **mask** is present, only the elements for which **mask** is _.true._
  are considered. 


### **Options**

- **array**
  : Shall be an array of type _integer_, _real_, or _character_.

- **dim**
  : (Optional) Shall be a scalar of type _integer_, with a value between
  one and the rank of **array**, inclusive. It may not be an optional
  dummy argument.

- **mask**
  : (Optional) Shall be an array of type _logical_, and conformable with
  **array**.

### **Result**

   If **dim** is absent, or if **array** has a rank of one, the result is
   a scalar.  If **dim** is present, the result is an array with a rank
   one less than the rank of **array**, and a size corresponding to the
   size of **array** with the **dim** dimension removed. In all cases,
   the result is of the same type and kind as **array**.

   If the considered array has zero size then the result is the most
   negative number of the type and kind of **array** if **array** is
   numeric, or a string of nulls if **array** is of ASCII character type.
   or equal to **CHAR(0, KIND(ARRAY))** otherwise.

### **Examples**

sample program:

```fortran
program demo_maxval
implicit none
integer,save :: ints(3,5)= reshape([&
   1,  2,  3,  4,  5, &
  10, 20, 30, 40, 50, &
  11, 22, 33, 44, 55  &
],shape(ints),order=[2,1])
character(len=:),allocatable :: strs(:)
integer :: i
character(len=*),parameter :: gen='(*(g0,1x))'
character(len=*),parameter :: ind='(3x,*(g0,1x))'

   print gen,'Given the array'
   write(*,'(1x,*(g4.4,1x))') &
   & (ints(i,:),new_line('a'),i=1,size(ints,dim=1))
   print gen,'Basics:'
   print ind, 'biggest value in array'
   print ind, maxval(ints)
   print ind, 'biggest value in each column'
   print ind, maxval(ints,dim=1)
   print ind, 'biggest value in each row'
   print ind,  maxval(ints,dim=2)

   print gen,'With a mask:'
   print ind, ' find biggest number less than 30 with mask'
   print ind, maxval(ints,mask=ints.lt.30)

   print gen,'If zero size considered:'
   print ind, 'if zero size numeric array'
   print ind, maxval([integer :: ]),'and -huge(0) is',-huge(0),&
   & '(often not the same!)'
   print ind, 'if zero-size character array all nulls'
   strs=[character(len=5)::]
   strs=maxval(strs)
   print ind, ichar([(strs(i),i=1,len(strs))])
   print ind, 'if everything is false,'
   print ind, 'same as zero-size array for each subarray'
   print ind, maxval(ints,mask=.false.)
   print ind, maxval(ints,mask=.false.,dim=1)
end program demo_maxval
```
Results:
```
 > Given the array:
 >    1,  2,  3,  4,  5, &
 >   10, 20, 30, 40, 50, &
 >   11, 22, 33, 44, 55  &
 > biggest value in array
 > 55
 > biggest value in each column
 > 11 22 33 44 55
 > biggest value in each row
 > 5 50 55
 > find biggest number less than 30 with mask
 > 22
 > if zero size numeric array
 > -2147483648 and -huge(0) is -2147483647 (often not the same!)
 > if zero-size character array all nulls
 > 0 0 0 0 0
 > if everything is false, same as zero-size array
 > -2147483648
 > -2147483648 -2147483648 -2147483648 -2147483648 -2147483648
```
### **Standard**

Fortran 95

### **See Also**

[**minval**(3)](#minval),
[**minloc**(3)](#minloc),
[**maxloc**(3)](#maxloc),
[**min**(3)](#min)
[**max**(3)](#max),

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
