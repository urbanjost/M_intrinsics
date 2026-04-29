## rewind

### **Name**

**rewind**(7) - \[FILE:POSITIONING\] rewind specified sequential
access I/O unit

### **Synopsis**
```fortran
    rewind file-unit-number

    rewind ( [UNIT=]file-unit-number][,IOMSG=iomsg-variable] &
    & [,IOSTAT=scalar-int-variable][,ERR=label] )
```
### **Description**

Execution of a REWIND(7) statement causes the file connected to the
specified unit to be positioned at the beginning of the file.

If the file is already positioned at its initial point, execution of
this statement has no effect on the position of the file.

Execution of a REWIND(7) statement for a file that is connected but
does not exist is permitted and has no effect on any file.

### **Options**

UNIT : unit number of file to rewind. A unit open for direct access or
stream access cannot be referenced by a REWIND(7) (e.g. you cannot
typically rewind stdin and stdout).

IOSTAT : (Optional) a compiler-specific number that indicates an error
occurred if non-zero. If not present and an error occurs the program
terminates.

IOMSG : (Optional) a message describing the error if IOSTAT is not zero.

ERR : (Optional) a label number to jump to if an error occurs

### **Examples**

An example of a REWIND(7) statement is:
```fortran
    program demo_rewind
    implicit none
    character(len=256) :: line
    character(len=256) :: mssge
    integer            :: i
    integer            :: ios
       open (10, file='demo_rewind.txt') ! open a file
       do i = 1, 100                     ! write lines to it
          write (10, '(a,i0)') 'line ', i
       enddo
       rewind (10, iostat=ios, iomsg=mssge)
       if (ios .ne. 0) then
          write (*, *) '*error* ', trim(mssge)
          stop
       endif
       write (*, *) 'wrote 100 lines, but now at line ...'
       read (10, '(a)') line
       write (*, '(a)') line
       read (10)
       read (10)
       read (10)
       write (*, *) 'skipped a few lines, now at ...'
       read (10, '(a)') line
       write (*, '(a)') line
       close (10, status='delete')
    end program demo_rewind
```
### **See Also**

The input/output statements are the OPEN(3), CLOSE(3), READ(3),
WRITE(3), PRINT(3), BACKSPACE(3), ENDFILE(3), REWIND(3), FLUSH(3),
WAIT(3) and INQUIRE(3) statements.

-   The READ(3) statement is a data transfer input statement.
-   The WRITE(3) statement and the PRINT(3) statement are data transfer
    output statements.
-   The WAIT(3) and FLUSH(3) statements are data transfer statements.
-   The OPEN(3) statement and the CLOSE(3) statement are file connection
    statements.
-   The INQUIRE(3) statement is a file inquiry statement.
-   The BACKSPACE(3), ENDFILE(3), and REWIND(3) statements are file
    positioning statements.

_Fortran statement descriptions (license: MIT) @urbanjost_
