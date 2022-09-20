## matmul

### **Name**

**matmul**(3) - \[TRANSFORMATIONAL\] numeric or logical matrix
multiplication

### **Syntax**
```fortran
function matmul(matrix_a, matrix_b)

 type(NUMERIC_OR_LOGICAL) :: matrix_a(..)
 type(NUMERIC_OR_LOGICAL) :: matrix_b(..)
 type(PROMOTED) :: matmul(..)
```
Arguments may be numeric (_integer_, _real_, or _complex_ )
or _logical_ one or two-dimensional arrays.

At least one argument must be rank two.

If one argument is _logical_, both must be _logical_.

### **Description**

**matmul**(3) performs a matrix multiplication on numeric or logical
arguments.

### **Arguments**

- **matrix_a**
  : A numeric or logical array with a rank of one or two.

- **matrix_b**
  : A numeric or logical array with a rank of one or two.  The last
  dimension of **matrix_a** and the first dimension of **matrix_b**
  must be equal.

  Note that **matrix_a** and **matrix_b** may be a different numeric
  types.

### **Returns**

####  **Numeric Arguments**

  If **matrix_a** and **matrix_b** are numeric the result is an
  array containing the conventional matrix product of **matrix_a**
  and **matrix_b**.

  First, for the numeric expression **C=matmul(A,B)**

   - Any vector **A(n)** is treated as a row vector **A(1,n)**.
   - Any vector **B(n)** is treated as a column vector **B(n,1)**.

#####  **Shape and Rank**

  The shape of the result can then be determined as the number of rows of
  the first matrix and the number of columns of the second; but if any
  resulting dimension is one the result is reduced to a rank one array
  (a vector). That is ...

   + If **matrix_a** has shape [n,m] and **matrix_b** has shape [m,k],
     the result has shape [n,k].
   + If **matrix_a** has shape [m] and **matrix_b** has shape [m,k],
     the result has shape [k].
   + If **matrix_a** has shape [n,m] and **matrix_b** has shape [m],
     the result has shape [n].

  Note this implies ...

    - when one of the arguments is of rank one, the result has a rank of one.
    - when both arguments are of rank two, the result has a rank of two.

#####  **Values**

  Then element **C(i,j)** of the product is obtained by multiplying
  term-by-term the entries of the ith row of **A** and the jth column
  of **B**, and summing these products. In other words, **C(i,j)**
  is the dot product of the ith row of **A** and the jth column of **B**.

#####  **Characteristics**

  The returned array will be promoted to the same type and kind as would
  result from multiplication between an element of each argument (like
  the **\*** operator had been used between the elements).

#### **Logical Arguments**

#####  **Values**

  If **matrix_a** and **matrix_b** are of type logical, the array elements
  of the result are instead:
```fortran
  Value_of_Element (i,j) = &
  ANY( (row_i_of_MATRIX_A) .AND. (column_j_of_MATRIX_B) )
```
#####  **Characteristics**

  The returned array is of the type and kind that results if any element of
  each argument had been operated on by the **.AND.** operator.

### **Examples**

Sample program:
```fortran
program demo_matmul
implicit none
integer :: a(2,3), b(3,2), c(2), d(3), e(2,2), f(3), g(2)
   a = reshape([1, 2, 3, 4, 5, 6], [2, 3])
   b = reshape([1, 2, 3, 4, 5, 6], [3, 2])
   c = [1, 2]
   d = [1, 2, 3]
   e = matmul(a, b)
   f = matmul(c,a)
   g = matmul(a,d)

   call print_matrix_int('A is ',a)
   call print_matrix_int('B is ',b)
   call print_vector_int('C is ',c)
   call print_vector_int('D is ',d)
   call print_matrix_int('E is matmul(A,B)',e)
   call print_vector_int('F is matmul(C,A)',f)
   call print_vector_int('G is matmul(A,D)',g)
contains

! CONVENIENCE ROUTINES TO PRINT IN ROW-COLUMN ORDER
subroutine print_vector_int(title,arr)
character(len=*),intent(in)  :: title
integer,intent(in)           :: arr(:)
   call print_matrix_int(title,reshape(arr,[1,shape(arr)]))
end subroutine print_vector_int

subroutine print_matrix_int(title,arr)
!@(#) print small 2d integer arrays in row-column format
character(len=*),parameter :: all='(" > ",*(g0,1x))' ! a handy format
character(len=*),intent(in)  :: title
integer,intent(in)           :: arr(:,:)
integer                      :: i
character(len=:),allocatable :: biggest

   print all
   print all, trim(title)
   biggest='           '  ! make buffer to write integer into
   ! find how many characters to use for integers
   write(biggest,'(i0)')ceiling(log10(real(maxval(abs(arr)))))+2
   ! use this format to write a row
   biggest='(" > [",*(i'//trim(biggest)//':,","))'
   ! print one row of array at a time
   do i=1,size(arr,dim=1)
      write(*,fmt=biggest,advance='no')arr(i,:)
      write(*,'(" ]")')
   enddo

end subroutine print_matrix_int

end program demo_matmul
```
Results:
```text
    >
    > A is
    > [  1,  3,  5 ]
    > [  2,  4,  6 ]
    >
    > B is
    > [  1,  4 ]
    > [  2,  5 ]
    > [  3,  6 ]
    >
    > C is
    > [  1,  2 ]
    >
    > D is
    > [  1,  2,  3 ]
    >
    > E is matmul(A,B)
    > [  22,  49 ]
    > [  28,  64 ]
    >
    > F is matmul(C,A)
    > [   5,  11,  17 ]
    >
    > G is matmul(A,D)
    > [  22,  28 ]
```

### **Standard**

Fortran 95 and later

### **See Also**

[**product**(3)](#product),
[**transpose**(3)](#transpose)

- [Matrix multiplication : Wikipedia](https://en.wikipedia.org/wiki/Matrix_multiplication)
- The Winograd variant of Strassen's matrix-matrix multiply algorithm may
  be of interest for optimizing multiplication of very large matrices. See
```text
    "GEMMW: A portable level 3 BLAS Winograd variant of Strassen's
    matrix-matrix multiply algorithm",

    Douglas, C. C., Heroux, M., Slishman, G., and Smith, R. M.,
    Journal of Computational Physics,
    Vol. 110, No. 1, January 1994, pages 1-10.

  The numerical instabilities of Strassen's method for matrix
  multiplication requires special processing.
```
 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
