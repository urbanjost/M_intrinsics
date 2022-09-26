## allocated

### **Name**

**allocated**(3) - \[ARRAY INQUIRY\] Status of an allocatable entity

### **Synopsis**
```fortran
    result = allocated(entity)
```
```fortran
     logical function allocated(entity)

     type(TYPE(kind=KIND)),allocatable :: entity(..)
```
  where **entity** may be any allocatable scalar or array object
  of any type.

### **Description**

  **allocated(arg)**  checks the allocation status of both arrays
  and scalars.

### **Options**

- **entity**
  : the _allocatable_ object to test.

### **Result**

  If the argument is allocated then the result is _.true._; otherwise,
  it returns _.false._.

### **Examples**

Sample program:

```fortran
program demo_allocated
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=sp), allocatable :: x(:)
character(len=256) :: message
integer :: istat
  ! basics
   if( allocated(x)) then
       write(*,*)'do things if allocated'
   else
       write(*,*)'do things if not allocated'
   endif

   ! if already allocated, deallocate
   if ( allocated(x) ) deallocate(x,STAT=istat, ERRMSG=message )
   if(istat.ne.0)then
      write(*,*)trim(message)
      stop
   endif

   ! only if not allocated, allocate
   if ( .not. allocated(x) ) allocate(x(20))

  ! allocation and intent(out)
   call intentout(x)
   write(*,*)'note it is deallocated!',allocated(x)

   contains

   subroutine intentout(arr)
   ! note that if arr has intent(out) and is allocatable,
   ! arr is deallocated on entry
   real(kind=sp),intent(out),allocatable :: arr(:)
       write(*,*)'note it was allocated in calling program',allocated(arr)
   end subroutine intentout

end program demo_allocated
```
Results:
```text
    T           4
    do things if allocated
    note it was allocated in calling program F
    note it is deallocated! F
```
### **Standard**

  Fortran 95 and later. Note, the scalar= keyword and allocatable
  scalar entities are available in Fortran 2003 and later.

### **See Also**

[**move_alloc**(3)](#move_alloc)

 _fortran-lang intrinsic descriptions_
