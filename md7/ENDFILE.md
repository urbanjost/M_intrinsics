## endfile

### **Name**

**endfile**(7) - \[NUMERIC\] Absolute value

### **Synopsis**
```fortran
    endfile unit_number
```
### **Description**

An **endfile**(7f) ends or truncates a file at the current record.

Execution of an **endfile**(7F) statement for a file connected for
SEQUENTIAL ACCESS writes an endfile record as the next record of the
file. The file is then positioned after the endfile record, which
becomes the last record of the file.

-  SEQUENTIAL ACCESS 
   : After execution of an **endfile**(7F) statement for
   a file connected for sequential access, a BACKSPACE(7F) or
   REWIND(7F) statement shall be used to reposition the file prior to
   execution of any data transfer input/output statement or **endfile**(7F)
   statement.

-  DIRECT ACCESS 
   : For a file connected for DIRECT ACCESS, only those
   records before the endfile record are considered to have been
   written. Thus, only those records or additional records subsequently
   written shall be read during subsequent direct access connections to
   the file.

-  STREAM ACCESS 
   : Execution of an **endfile**(7F) statement for a file
   connected for STREAM ACCESS causes the terminal point of the file to
   become equal to the current file position. Only file storage units
   before the current position are considered to have been written;
   thus only those file storage units shall be subsequently read.
   Subsequent stream output statements may be used to write further
   data to the file.

Execution of an **endfile**(7F) statement for a file that is connected but
does not exist creates the file; if the file is connected for sequential
access, it is created prior to writing the endfile record.

### **Options**
    LUN   A unit number of a connected file

### **Examples**

An example of an **endfile**(7f) statement is:
```fortran
    program demo_endfile
    implicit none
    integer :: lun, i, j, iostat
    integer,parameter:: isz=10
       !
       ! create a little scratch file
       open(newunit=lun,file='_scr.txt', form='formatted')
       write(lun,'(i0)')(100+i,i=1,isz)
       !
       ! write end of file after reading half of file
       rewind(lun)
       write(*,*)'rewind and read',isz/2,'lines'
       read(lun,*)(j,i=1,isz/2)
       endfile lun ! will truncate line at current position
       !
       ! NOTE: backspace before writing any addition lines
       !       once an ENDFILE(7f) statement is executed
       ! backspace(lun)
       !
       ! rewind and echo remaining file
       rewind(lun)
       j=0
       do i=1,huge(0)-1
          read(lun,*,iostat=iostat)j
          if(iostat.ne.0)exit
          write(*,*)i,j
       enddo
       write(*,*)'number of lines in file was ',isz,', is now ',i-1
       close(unit=lun,status='delete')
    end program demo_endfile
```
### **See Also**

[**backspace**(7)](#backspace),
[**close**(7)](#close),
[**endfile**(7)](#endfile),
[**flush**(7)](#flush),
[**inquire**(7)](#inquire),
[**open**(7)](#open),
[**print**(7)](#print),
[**read**(7)](#read),
[**rewind**(7)](#rewind),
[**wait**(7)](#wait),
[**write**(7)](#write)
