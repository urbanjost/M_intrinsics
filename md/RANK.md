## rank

### **Name**

**rank**(3) - \[ARRAY INQUIRY\] Rank of a data object

### **Synopsis**
```fortran
    result = rank(a)
```
```fortran
     integer function rank(a)

     type(TYPE(kind=KIND),intent(in) :: a(..)
```
### **Characteristics**

  **a** can be of any type and rank.

### **Description**

  **rank(a)** returns the rank of a scalar or array data object.

### **Options**

- **a**

### **Result**

  For arrays, their rank is returned; for scalars zero is returned.

### **Examples**

Sample program:

```fortran
program demo_rank
implicit none
integer :: a
real, allocatable :: b(:,:)
real  :: c(10,20,30)
complex :: d
! make up a type
type mytype
   integer :: int
   real :: float
   character :: char
end type mytype
type(mytype) :: any_thing(1,2,3,4,5)

   print *, 'rank of scalar a=',rank(a)
   ! note you can query this array even though not allocated
   print *, 'rank of matrix b=',rank(b)
   print *, 'rank of vector c=',rank(c)
   print *, 'rank of scalar d=',rank(d)
   ! you can query any type
   print *, 'rank of any_thing=',rank(any_thing)

   call query_int(10)
   call query_int([20,30])
   call query_int( reshape([40,50,60,70],[2,2]) )

contains

subroutine query_int(entity)
! It is hard to do much with something dimensioned
! name(..) if not calling C but one thing you can
! do is call the inquiry functions ...
integer,intent(in) :: entity(..)

   if(rank(entity).eq.0)then
      write(*,*)'you passed a scalar',rank(entity)
   else
      write(*,*)'you passed an array, rank=',rank(entity)
   endif

end subroutine query_int

end program demo_rank
```
  Results:
```text
    rank of scalar a= 0
    rank of matrix b= 2
    rank of vector c= 3
    rank of scalar d= 0
    rank of any_thing= 5
    you passed a scalar 0
    you passed an array, rank= 1
    you passed an array, rank= 2
```
### **Standard**

### **See also**

[****(3)](#)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
