## is_iostat_eor

### **Name**

**is_iostat_eor**(3) - \[STATE:INQUIRY\] Test for end-of-record value

### **Synopsis**
```fortran
    result = is_iostat_eor(i)
```
```fortran
     elemental integer function is_iostat_eor(i)

      integer(kind=KIND),intent(in) :: i
```
### **Characteristics**

 - **i** is _integer_ of any kind
 - the return value is a default _logical_

### **Description**

  **is_iostat_eor**(3) tests whether a variable has the value of the
  I/O status "end of record". The function is equivalent to comparing
  the variable with the **iostat_eor** parameter of the intrinsic module
  **iso_fortran_env**.

### **Options**

- **i**
  : The value to test as indicating "end of record".

### **Result**

  Returns _.true._ if and only if **i** has the value which indicates
  an end-of-record condition for iostat= specifiers, and is _.false._
  otherwise.

### **Examples**

Sample program:

```fortran
program demo_is_iostat_eor
use iso_fortran_env, only : iostat_eor
implicit none
integer :: inums(5), lun, ios

  ! create a test file to read from
   open(newunit=lun, form='formatted',status='scratch',action='readwrite')
   write(lun, '(a)')     &
   '10   20   30',       &
   '40   50   60   70',  &
   '80   90',            &
   '100',                &
   '110 120 130',        &
   '140'
   rewind(lun)

   do
      read(lun, *, iostat=ios) inums
      write(*,*)'iostat=',ios
      if(is_iostat_eor(ios)) then
         inums=-huge(0)
         print *, 'end of record'
      elseif(is_iostat_end(ios)) then
         print *,'end of file'
         inums=-huge(0)
         exit
      elseif(ios.ne.0)then
         print *,'I/O error',ios
         inums=-huge(0)
         exit
      else
         write(*,'(*(g0,1x))')'inums=',inums
      endif
   enddo

   close(lun,iostat=ios,status='delete')

end program demo_is_iostat_eor
```
Results:
```text
 >  iostat=           0
 > inums= 10 20 30 40 50
 >  iostat=           0
 > inums= 80 90 100 110 120
 >  iostat=          -1
 >  end of file
```
Note:
the list-directed read starts on a new line with each read, and
that the read values should not portably be used if IOSTAT is not zero.

Format descriptors, Stream I/O and non-advancing I/O and reads into
strings that can then be parsed or read multiple times give full control
of what is read. List-directed I/O is generally more appropriate for
interactive I/O.
### **Standard**

Fortran 2003

### **See also**

 - [associated(3)](#associated) -  Association status of a pointer or pointer/target pair
 - [extends_type_of(3)](#extends_type_of) -  Determine if the dynamic type of A is an extension of the dynamic type of MOLD.
 - [is_iostat_end(3)](#is_iostat_end) -  Test for end-of-file value
 - [is_iostat_eor(3)](#is_iostat_eor) -  Test for end-of-record value
 - [present(3)](#present)   -  Determine whether an optional dummy argument is specified
 - [same_type_as(3)](#same_type_as) -  Query dynamic types for equality

 _Fortran intrinsic descriptions_
