## print

### **Name**
   print(7f) - [FORTRAN:IO] write formatted sequential I/O to stdout
### **Synopsis**
```fortran
    PRINT format [ , output-item-list ]
```
### **Description**
    print(7f) is equivalent to 
```fortran
    write(*,fmt=FORMAT_SPECIFIER) LIST
```
    That is, it always writes formatted sequential I/O to stdout. It
    may use list-directed I/O or a FORMAT specifier.

    print(7f) allows for no other options and therefore cannot be used
    for binary or non-advancing or stream or asynchronous I/O or any
    of the other options provided by the more general but also more
    complicated write(7f) statement.

    Note that pure subprograms cannot contain I/O statements such as
    print(7f).

### **Options**
    format            a format may be used to specify how output items
		      are displayed using the many Fortran format
		      descriptors, or an asterisk (*) may be used to
		      indicate to use list-directed default formatting.
    output-item-list  the variables whose values are to be displayed

### **Example**

  A simple example program:
```fortran
   program demo_print
   implicit none
   real :: a=11.11, s=sqrt(12.0)
   integer :: j=753210
   character(len=*),parameter :: commas='(*(g0:,","))'

  ! List-directed output is frequently specified
   PRINT *, A, S

  ! a format may be placed on the print(7f) statement
   PRINT '(*(g0,1x))', A, S, J

  ! the format may be in a character variable
   print commas, a, s, j

  ! or may be in a labeled format statement
   PRINT 10, A, S, J
   10 FORMAT (2E16.3,1x,I0)

   end program demo_print
```
Results:
```text
 >    11.1099997       3.46410155    
 > 11.1099997 3.46410155 753210
 > 11.1099997,3.46410155,753210
 >        0.111E+02       0.346E+01 753210
```
### **See Also**

 - [**backspace**(3)](#backspace)
 - [**close**(3)](#close)
 - [**endfile**(3)](#endfile)
 - [**flush**(3)](#flush)
 - [**inquire**(3)](#inquire)
 - [**open**(3)](#open)
 - [**print**(3)](#print)
 - [**read**(3)](#read)
 - [**rewind**(3)](#rewind)
 - [**wait**(3)](#wait)
 - [**write**(3)](#write)

_fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
