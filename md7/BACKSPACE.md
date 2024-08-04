## backspace

### **Name**
   BACKSPACE(7f) - [FORTRAN:IO:FILE POSITIONING] - backspace one record on
   specified I/O unit

### **Synopsis**
```fortran
BACKSPACE file-unit-number
```
```fortran
BACKSPACE([UNIT=]unit-number][,IOMSG=iomsg][,IOSTAT=iostat][,ERR=label])
```
### **Description**
   backspace(7f) positions the specified file back to the beginning
   of the current record or if already at the beginning of a record,
   back to the beginning of the previous record.

   If the file is at its initial point, the position of the file is
   not changed.

   It is most often used when a program has partially read a line and
   then wants to go back and reread the line using the information from
   the previous read(7f),

   backspace(7f) is rarely used in new code as the subsequent addition
   of Fortran features such as non-advancing I/O and internal reads
   into a CHARACTER variable (which can be read from multiple times) are
   typically far more efficient and provide much of the same functionality
   when re-reading the current line.

   Backspacing is very inefficient on many current platforms. Reading a
   file with stream-I/O and indexing relevant line positions to return to;
   or using direct-access files is far more efficient than backspacing
   through a file when moving back large numbers of lines on Linux and
   Unix platforms.

   A unit open for direct access or unformatted access cannot be
   referenced by backspace(7f).  backspace(7f) only works with formatted
   sequential files that may be repositioned. So it does not generally
   work with standard input from a terminal, pipes, and other formatted
   sequential file types that cannot be rewound or positioned.

   Backspacing over records written using list-directed or namelist
   formatting is prohibited. It will usually work, but since the compiler
   is free to write list-directed or namelist output on a varying number
   of lines it is not supported, as it is not certain what data is on
   which line unless the program itself searches for particular strings.

   Backspacing a file that is connected but does not exist is prohibited.

   If a BACKSPACE statement causes the implicit writing of an endfile
   record, the file is positioned before the record that precedes the
   endfile record.

   If the preceding record is an endfile record, the file is positioned
   before the endfile record.

### **Options**
   **unit**
   : unit number of file to backspace one line on.
     A unit open for direct access or unformatted access cannot
     be referenced by a BACKSPACE.
   **iostat**
   : a compiler-specific number that indicates an error occurred
     if non-zero.
   **iomsg**
   : a message describing error IOSTAT if IOSTAT is not zero.
   **err**
   : a label number to jump to if an error occurs

### **Example**
  An example of a BACKSPACE statement is:
```fortran
   program demo_backspace
   implicit none
   character(len=256) :: line
   character(len=256) :: mssge
   integer            :: i
   integer            :: j
   integer            :: ios
   integer,allocatable :: iarr(:)

      ! create a basic sequential file
      open(10,file='dem_backspace.txt') ! open a file
      do i=1,30                         ! write lines to it
         write(10,'(a,i3,*(i3))') 'line ',i, (j,j=1,i)
      enddo

      ! back up several lines
      do i=1,14
         backspace(10, iostat=ios,iomsg=mssge)
         if(ios.ne.0)then
                 write(*,'(*(a))') '*dem_backspace* ERROR:',mssge
         endif
      enddo
      read(10,'(a)')line
      write(*,*)'back at a previous record !'

      ! read line as a string
      write(*,'("string=",a)')trim(line)

      ! backspace so can read again as numbers
      backspace(10)
      ! read part of a line numerically to get size of array to read
      read(10,'(5x,i3)')i
      allocate(iarr(i))

      ! reread line just reading array
      backspace(10)
      read(10,'(8x,*(i3))')iarr
      write(*,'(*(g0,1x))')'size=',i,'array=',iarr

      !! Note: writing a new line will truncate file
      !!       to current record position

      close(10,status='delete')

   end program demo_backspace
```
Results:
```text
 >  back at a previous record !
 > string=line  17  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
 > size= 17 array= 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17
```

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
