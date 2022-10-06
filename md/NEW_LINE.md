## new_line

### **Name**

**new_line**(3) - \[CHARACTER\] New-line character

### **Synopsis**
```fortran
    result = new_line(c)
```
```fortran
     character(len=1,kind=kind(c)) function new_line(c)

      character(len=1,kind=KIND),intent(in) :: c(..)
```
### **Characteristics**

### **Description**

**new_line**(3) returns the new-line character.

Normally, new-lines are generated with regular formatted I/O statements like
WRITE() and PRINT() when each statement completes:
```fortran
   print *, 'x=11'
   print *
   print *, 'y=22'
   end
```
produces:
    x=11

    y=22
```
Alternatively, a "/" descriptor in a format is used to generate a
newline on the output. For example:
```fortran
   write(*,'(a,1x,i0,/,a)') 'x =',11,'is the answer'
   end
```
produces:
```text
   x = 11
   is the answer
```
Also, for formatted sequential output if more data is listed on the
output statement than can be represented by the format statement a
newline is generated and then the format is reused until the output
list is exhausted.
```fortran
   write(*,'(a,"=",i0)') 'x', 10, 'y', 20
   end
```
produces
```text
   x=10
   y=20
```
But there are occasions, particularly when non-advancing I/O or stream
I/O is being generated (which does not generate a new-line at the end
of each WRITE statement, as normally occurs) where it is preferable to
place a new-line explicitly in the output at specified points.

To do so you must make sure you are generating the correct new-line
character, which the techniques above do automatically.

The new-line character varies between some platforms, and can even
depend on the encoding (ie. which character set is being used) of the
output file.  In these cases selecting the correct character to output
can be determined by the **new_line**(3) procedure.

Case (i)
  : If **a** is default _character_ and the character in position **10**
  of the ASCII collating sequence is representable in the default
  character set, then the result is **achar(10)**.

  This is the typical case, and just requires using "new_line('a')".

Case (ii)
  : If **a** is an ASCII character or an ISO 10646 character, then the
  result is **char(10, kind (a))**.

Case (iii)
  : Otherwise, the result is a processor-dependent character that
  represents a newline in output to files connected for formatted
  stream output if there is such a character.

Case (iv)
  : If not of the previous cases apply, the result is the blank character.

### **Options**

- **c**
  : an arbitrary character whose kind is used to decide on the output
  character that represents a new-line.

### **Result**

Returns a _character_ scalar of length one with the new-line character of
the same kind as parameter **c**.

### **Examples**

Sample program:
```fortran
program demo_new_line
implicit none
character,parameter :: nl=new_line('a')
character(len=:),allocatable :: string
real :: r
integer :: i, count

  ! basics
   ! print a string with a new-line embedded in it
   string='This is record 1.'//nl//'This is record 2.'
   write(*,'(a)') string

   ! print a new-line character string
   write(*,'(*(a))',advance='no') &
      nl,'This is record 1.',nl,'This is record 2.',nl

   ! output a number of words of random length as a paragraph
   ! by inserting a new_line before line exceeds 70 characters

  ! simplistic paragraph print using non-advancing I/O
   count=0
   do i=1,100

      ! make some fake word of random length
      call random_number(r)
      string=repeat('x',int(r*10)+1)

      count=count+len(string)+1
      if(count.gt.70)then
         write(*,'(a)',advance='no')nl
         count=len(string)+1
      endif
      write(*,'(1x,a)',advance='no')string
   enddo
   write(*,'(a)',advance='no')nl

end program demo_new_line
```
  Results:
```text
   This is record 1.
   This is record 2.

   This is record 1.
   This is record 2.
    x x xxxx xxxxxxx xxxxxxxxxx xxxxxxxxx xxxx xxxxxxxxxx xxxxxxxx
    xxxxxxxxx xxxx xxxxxxxxx x xxxxxxxxx xxxxxxxx xxxxxxxx xxxx x
    xxxxxxxxxx x x x xxxxxx xxxxxxxxxx x xxxxxxxxxx x xxxxxxx xxxxxxxxx
    xx xxxxxxxxxx xxxxxxxx x xx xxxxxxxxxx xxxxxxxx xxx xxxxxxx xxxxxx
    xxxxx xxxxxxxxx x xxxxxxxxxx xxxxxx xxxxxxxx xxxxx xxxxxxxx xxxxxxxx
    xxxxx xxx xxxxxxxx xxxxxxx xxxxxxxx xxx xxxx xxx xxxxxxxx xxxxxx
    xxxxxxx xxxxxxx xxxxx xxxxx xx xxxxxx xx xxxxxxxxxx xxxxxx x xxxx
    xxxxxx xxxxxxx x xxx xxxxx xxxxxxxxx xxx xxxxxxx x xxxxxx xxxxxxxxx
    xxxx xxxxxxxxx xxxxxxxx xxxxxxxx xxx xxxxxxx xxxxxxx xxxxxxxxxx
    xxxxxxxxxx xxxxxx xxxxx xxxx xxxxxxx xx xxxxxxxxxx xxxxxx xxxxxx
    xxxxxx xxxx xxxxx
```
### **Standard**

Fortran 2003

### **See also**

[**achar**(3)](#achar),
[**char**(3)](#char),
[**iachar**(3)](#iachar),
[**ichar**(3)](#ichar),
[**selected_char_kind**(3)](#selected_char_kind)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
