## new_line

### **Name**

**new_line**(3) - \[CHARACTER:WHITESPACE\] Newline character

### **Synopsis**
```fortran
    result = new_line(c)
```
```fortran
     character(len=1,kind=KIND) function new_line(c)

      character(len=1,kind=KIND),intent(in) :: c(..)
```
### **Characteristics**

 - **c** shall be of type _character_. It may be a scalar or an array.
 - the result is a _character_ scalar of length one with the same kind type parameter as **c**.

### **Description**

**new_line**(3) returns the newline character.

Normally, newlines are generated with regular formatted I/O statements like
WRITE() and PRINT() when each statement completes:
```fortran
   print *, 'x=11'
   print *
   print *, 'y=22'
   end
```
produces:
```text
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
I/O is being generated (which does not generate a newline at the end
of each WRITE statement, as normally occurs) where it is preferable to
place a newline explicitly in the output at specified points.

To do so you must make sure you are generating the correct newline
character, which the techniques above do automatically.

The newline character varies between some platforms, and can even
depend on the encoding (ie. which character set is being used) of the
output file. In these cases selecting the correct character to output
can be determined by the **new_line**(3) procedure.

### **Options**

- **c**
  : an arbitrary character whose kind is used to decide on the output
  character that represents a newline.

### **Result**

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

### **Examples**

Sample program:
```fortran
program demo_new_line
implicit none
! Get the system's newline character
character,parameter          :: nl=new_line('a')
character(len=:),allocatable :: string
real                         :: r
integer                      :: i, count
integer                      :: u, pos_save
character(len=256)           :: line_buffer

  ! basics
   ! print a string with a newline embedded in it
   string='This is record 1.'//nl//'This is record 2.'
   write(*,'(a)') string

  ! Non-Advancing I/O with Newline 
   ! Combining ADVANCE='NO' with NEW_LINE allows for granular control
   ! over output formatting.
   ! print a newline character string
   write(*,'(*(a))',advance='no') &
      nl,'This is record 1.',nl,'This is record 2.',nl

  ! Stream I/O

    ! 1. Open a file for formatted stream output
    open(newunit=u, file='test_stream.txt', access='stream', &
         form='formatted', status='replace')

    ! 2. Write data with manual newlines
    write(u, '(A)') 'First Line' // nl
    
    ! Inquire current position (byte offset) before writing second line
    inquire(unit=u, pos=pos_save)
    
    write(u, '(A)') 'Second Line' // nl
    write(u, '(A)') 'Third Line' // nl
    
    ! Jump directly to the saved position (start of the second line)
    read(u, '(A)', pos=pos_save) line_buffer
    print *, 'Data read from saved position:', trim(line_buffer)

    close(u)

  ! Extended Example Providing Paragraph Fill
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
 > This is record 1.
 > This is record 2.
 > 
 > This is record 1.
 > This is record 2.
 >  Data read from saved position:Second Line
 >  xxxxxx xx xxxxxxx xxxx xxxxx x xxxxx xxxxx xxxxxxxxxx xxxxxxx xxxxxxx
 >  xxx xx xxxxxxxxxx xxxxxx x xx xxxx xxxxxxx x xxxxxxxxxx xxxxxx
 >  xxxxxxx xxxx xxxxxxxxxx xxx xxxxxxxxx xxxxxxx xx xxxxxxxxxx x
 >  xxxxxxxxxx xxxxxxxxx x xxx xxxx xxxxxxxxx xx xxxxxxxx xxx xxxxxxx x x
 >  xxxx xxxxx xxxxxx xxxxxxxxx xxxxxxxxx xxxxxx x xxxxxxxxx x xx xxxxxxx
 >  xxx xxxxxx xxxxx xxxxxxxx xxxxxxxxxx xx xx xxxxxxxxxx xxxxxxxxxx
 >  xxxxxx xxxx xxxxxxx xxxxxx xxxxxx xx xxxxxxxx xxxxxxxx xxx xxxxxxxx
 >  xxxxxxxxx xxxxxx xxxxxxxxx xx xxxxxxxxx xxxxx xx xxxxxxx xxxxxxxxx
 >  xxxxxxxxx xxxx xxxxxxxxxx xxx xxxxxxxxx xxxxxxxxxx x xxxxxx xxxxxx
 >  xxxxxxxxxx x xxxxx xx xxxxxxx xxxxxxx xxxxxx xxxxx xxxxxxx
```
### **Standard**

Fortran 2003

### **See also**

[**achar**(3)](#achar),
[**char**(3)](#char),
[**iachar**(3)](#iachar),
[**ichar**(3)](#ichar),
[**selected_char_kind**(3)](#selected_char_kind)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
