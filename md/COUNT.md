## count

### **Name**

**count**(3) - \[ARRAY REDUCTION\] Count function

### **Syntax**
```fortran
    result = count(mask [,dim] [,kind] )
```
```fortran
    integer(kind=KIND) function count(mask, dim, kind )
    logical(kind=KINDL),intent(in) :: mask(..)
    integer(kind=KINDD),intent(in),optional :: dim
    integer(kind=KINDK),intent(in),optional :: kind
```
The return value is of type _integer_ and of kind **kind**. If **kind**
is absent, the return value is of default integer kind.

If **dim** is present, the result is an array with a rank one less than
the rank of **array**, and a size corresponding to the shape of **array**
with the **dim** dimension removed.

### **Description**

Counts the number of _.true._ elements in a logical **mask**, or, if the **dim**
argument is supplied, counts the number of elements along each row of
the array in the **dim** direction. If the array has zero size, or all of
the elements of **mask** are false, then the result is **0**.

### **Arguments**

- **mask**
  : The type shall be _logical_.

- **dim**
  : (Optional) The type shall be _integer_.

- **kind**
  : (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Returns**

The return value is of type _integer_ and of kind **kind**. If **kind** is absent,
the return value is of default integer kind. If **dim** is present, the
result is an array with a rank one less than the rank of **array**, and a
size corresponding to the shape of **array** with the **dim** dimension removed.

### **Examples**

Sample program:

```fortran
program demo_count
implicit none
! two arrays and a mask all with the same shape
integer, dimension(2,3) :: a, b
logical, dimension(2,3) :: mymask
   a = reshape( [ 1, 2, 3, 4, 5, 6 ], [ 2, 3 ])
   b = reshape( [ 0, 7, 3, 4, 5, 8 ], [ 2, 3 ])
   ! show numeric arrays we will compare
   print '(3i3)', a(1,:)
   print '(3i3)', a(2,:)
   print *
   print '(3i3)', b(1,:)
   print '(3i3)', b(2,:)

   ! basic calls
   print *, 'count a few basic things ...'
   print *, 'count a>b',count(a>b)
   print *, 'count b<a',count(a<b)
   print *, 'count b==a',count(a==b)
   print *, 'check sum = ',count(a>b) + &
                         & count(a<b) + &
                         & count(a==b).eq.size(a)

   ! this is easier to visualize if we look at a mask
   ! make a mask identifying unequal elements
   mymask = a.ne.b
   print *, 'show mask for a.ne.b'
   print '(3l3)', mymask(1,:)
   print '(3l3)', mymask(2,:)
   ! count total and along rows and columns
   print '(a)', 'number of elements not equal'
   print '(a)', '(ie. total true elements in the mask)'
   print '(3i3)', count(mymask)
   print '(a)', 'count of elements not equal in each column'
   print '(a)', '(ie. total true elements in each column)'
   print '(3i3)', count(mymask, dim=1)
   print '(a)', 'count of elements not equal in each row'
   print '(a)', '(ie. total true elements in each row)'
   print '(3i3)', count(mymask, dim=2)

end program demo_count
```
Results:
```text
     1  3  5
     2  4  6
    
     0  3  5
     7  4  8
    count a few basic things ...
    count a>b           1
    count b<a           2
    count b==a           3
    check sum =  T
    show mask for a.ne.b
     T  F  F
     T  F  T
   number of elements not equal
   (ie. total true elements in the mask)
     3
   count of elements not equal in each column
   (ie. total true elements in each column)
     2  0  1
   count of elements not equal in each row
   (ie. total true elements in each row)
     1  2
```
### **Standard**

Fortran 95 and later, with KIND argument - Fortran 2003
and later

 _fortran-lang intrinsic descriptions_
