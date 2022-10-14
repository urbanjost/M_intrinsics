## is_iostat_eor

### **Name**

**is_iostat_eor**(3) - \[STATE:INQUIRY\] Test for end-of-record value

### **Synopsis**
```fortran
    result = is_iostat_eor(i)
```
```fortran
     elemental integer function is_iostat_eor(i)

      integer(kind=KIND),intent(in) :: iostat
```
### **Characteristics**

### **Description**

**is_iostat_eor**(3) tests whether a variable has the value of the I/O
status "end of record". The function is equivalent to comparing the
variable with the **iostat_eor** parameter of the intrinsic module
**iso_fortran_env**.

### **Options**

- **i**
  : The value to test as indicating "end of record".

### **Result**

Returns a _logical_ of the default kind, which is _.true._ if **i**
has the value which indicates an end of file condition for iostat=
specifiers, and is _.false._ otherwise.

### **Examples**

Sample program:

```fortran
program demo_is_iostat_eor
use iso_fortran_env, only : iostat_eor
implicit none
integer :: inums(50), lun, ios

  open(newunit=lun, file='_test.dat', form='unformatted')
  write(lun, '(a)') '10 20 30'
  write(lun, '(a)') '40 50 60 70'
  write(lun, '(a)') '80 90'
  write(lun, '(a)') '100'

  do
     read(lun, *, iostat=ios) inums
     write(*,*)'iostat=',ios
     if(is_iostat_eor(ios)) stop 'end of record'
  enddo

  close(lun,iostat=ios,status='delete')

end program demo_is_iostat_eor
```
### **Standard**

Fortran 2003

### **See also**

[****(3)](#)

 _fortran-lang intrinsic descriptions_
