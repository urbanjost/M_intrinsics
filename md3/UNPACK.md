## unpack

### **Name**

**unpack**(3) - \[ARRAY:CONSTRUCTION\] Scatter the elements of a vector
into an array using a mask

### **Synopsis**
```fortran
    result = unpack(vector, mask, field)
```
```fortran
     type(TYPE(kind=KIND)) unpack(vector, mask, field)

      type(TYPE(kind=KIND)),intent(in) :: vector(:)
      logical,intent(in)               :: mask(..)
      type(TYPE(kind=KIND)),intent(in) :: field(..)
```
### **Characteristics**

 - **vector** is a rank-one array of any type
 - **mask** is a logical array
 - **field** is the same type and type parameters as VECTOR conformable with **mask**.
 - The result is an array of the same type and type parameters as **vector**
   and the same shape as **mask**.

### **Description**

**unpack**(3) scatters the elements of **vector** into a copy of an
array **field** of any rank using _.true._ values from **mask** in array
element order to specify placement of the **vector** values.

The result is a copy of **field** generated with select elements
replaced with values from **vector**.

That is, **field** and **mask** are of the same shape. A copy of
**field** is made except that where any element of **mask** is .true. the
corresponding element in **field** is replaced with the next value
in **vector**.

This allows for complex replacement patterns
that would be difficult when using array syntax or multiple assignment
statements, particularly when the replacements are conditional.

### **Options**

- **vector**
  : New values to place into specified locations in **field**.
  It shall have at least as many elements as **mask** has _.true._
  values.

- **mask**
  : Shall be an array that specifies which values
  in **field** are to be replaced with values from **vector**.

- **field**
  : The input array to be altered, or a scalar.

### **Result**

  The element of the result that corresponds to the ith true element of
  **mask**, in array element order, has the value **vector(i)** for i =
  1, 2, .., N, where N is the number of true values in **mask**. Each
  other element has a value equal to **field** if **field** is scalar
  or to the corresponding element of **field** if it is an array.

  The resulting array corresponds to **field** with _.true._ elements
  of **mask** replaced by values from **vector** in array element order.

### **Examples**

Sample program:
```fortran
program demo_unpack
implicit none
logical,parameter :: T=.true., F=.false.
integer,parameter :: rows=3, cols=3
integer           :: i
logical           :: mask(rows,cols) = reshape([ &
   T, F, F, &
   F, T, F, &
   F, F, T  &
],[3,3])
integer :: field(rows,cols) = reshape([ &
   1, 2, 3, &
   4, 5, 6, &
   7, 8, 9  &
],[3,3])
integer :: result(rows,cols)

  ! mask and field must conform or field must be a scalar
   write(*,*) 'if the logical mask is'
   do i=1,size(mask,dim=1)
      write(*,*)mask(i,:)
   enddo
   write(*,*) 'and field is a scalar (in this case, 0)'
   write(*,*) 'the result is the shape of the mask'
   write(*,*) 'with all values set to the scalar value'
   write(*,*) 'except the true elements of the mask are'
   write(*,*) 'filled in row-column order with values'
   write(*,*) 'from the vector of values [11,22,33]'
   result = unpack( [11,22,33], mask, field=0 )
   call print_matrix_int('result=', result)

   write(*,*) 'if field is an array it must conform'
   write(*,*) 'to the shape of the mask'
   call print_matrix_int('field=',field)
   write(*,*) 'and the combination results in'
   result = unpack( [11,22,33], mask, field )
   call print_matrix_int('result=', result)

contains

subroutine print_matrix_int(title,arr)
! @(#) convenience routine:
!      prints small integer arrays in row-column format
implicit none
character(len=*),intent(in)  :: title
integer,intent(in)           :: arr(:,:)
integer                      :: i
character(len=:),allocatable :: biggest

   write(*,*)trim(title)
   ! make buffer to write integer into
   biggest='           '
   ! find how many characters to use for integers
   write(biggest,'(i0)')ceiling(log10(max(1.0,real(maxval(abs(arr))))))+2
   ! use this format to write a row
   biggest='("  [",*(i'//trim(biggest)//':,","))'
   ! print one row of array at a time
   do i=1,size(arr,dim=1)
      write(*,fmt=biggest,advance='no')arr(i,:)
      write(*,'(" ]")')
   enddo
end subroutine print_matrix_int

end program demo_unpack
```
Results:
```text
 >  if the logical mask is
 >  T F F
 >  F T F
 >  F F T
 >  and field is a scalar (in this case, 0)
 >  the result is the shape of the mask
 >  with all values set to the scalar value
 >  except the true elements of the mask are
 >  filled in row-column order with values
 >  from the vector of values [11,22,33]
 >  result=
 >   [  11,   0,   0 ]
 >   [   0,  22,   0 ]
 >   [   0,   0,  33 ]
 >  if field is an array it must conform
 >  to the shape of the mask
 >  field=
 >   [  1,  4,  7 ]
 >   [  2,  5,  8 ]
 >   [  3,  6,  9 ]
 >  and the combination results in
 >  result=
 >   [  11,   4,   7 ]
 >   [   2,  22,   8 ]
 >   [   3,   6,  33 ]
```
### **Standard**

Fortran 95

### **See Also**

[**merge**(3)](#merge),
[**pack**(3)](#pack),
[**spread**(3)](#spread)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
