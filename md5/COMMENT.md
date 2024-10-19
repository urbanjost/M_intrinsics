## comment

### **Name**

**comment**(5) - \[SYNTAX\] code annotation

### **Synopsis**
```fortran
    C fixed-format comment
    ! free-format comment
```
### **Description**

   In free-format files The character "!" initiates a comment except
   when it appears in a character context as part of a literal string.

   The comment extends to the end of the line.

   If the first nonblank character on a line is an "!", the line is
   a "comment line". Lines containing only blanks or containing no
   characters are also comment lines.

   "comment lines" may appear anywhere. There are a few restrictions
   on comments trailing statements or continued statements though.

   Comments may appear anywhere in a program unit and may precede the
   first statement of a program unit or follow the last statement of a
   program unit.

   Comments have no effect on the interpretation of the program unit.

   A continued line ending in an ampersand can additionally be followed
   by an exclamation and remarks unless a literal string is being
   continued.

   Comment lines cannot be continued with an ampersand. An ampersand
   appearing in a comment has no special effect and is merely a regular
   character.

   The standard does not restrict the number of consecutive comment lines.

   **FIXED-FORMAT SOURCE FILES**

   Additionally, in fixed-format source files a "C" in column 1 indicates
   the remainder of the line is a comment. An asterisk "*" in column 1
   beginning a comment is a common extension as well.

   There is a conflict in fixed-format files that can occur with the
   otherwise universal rule that at exclamation outside of a literal
   string begins a comment -- If the first non-blank character in a
   line is in column 6 in a fixed-format file it is a continuation line,
   not a comment. This rule includes an exclamation character as well.

   **FREE FORM COMMENTARY**

   A comment is explanatory text embedded in program source intended to
   help human readers understand it.

   Code completely without comments is often hard to read, but code
   with too many comments is also bad, especially if the comments are
   not kept up-to-date with changes to the code.

   Too much commenting may mean that the code is over-complicated.

   A good rule is to comment everything that needs it but write code
   that doesn't need much of it.

   Comments that explain __why__ something is done and how the code
   relates to its environment are useful.

   A particularly irksome form of over-commenting explains exactly
   what each statement does, even when it is obvious to any reasonably
   competant programmer.

### **Examples**

Sample program:
```fortran
program demo_comment
integer :: values(8)
character(len=:),allocatable :: string
character(len=1),parameter   :: dash='-',colon=':',dot='.'
real :: x=3.0, y=4.0
   ! comments may appear on a continued line
   ! blank lines are comment lines
   call date_and_time(values=values)
   associate( &

    ! DATE
    YR=>values(1),      & ! The year
    MO=>values(2),      & ! The month
    DY=>values(3),      & ! The day of the month

    ! TIME
    UTC=>values(4),     & ! Time difference with UTC in minutes
    HR=>values(5),      & ! The hour of the day
    MIN=>values(6),     & ! The minutes of the hour
    SEC=>values(7),     & ! The seconds of the minute
    MILLI=>values(8) )    ! The milliseconds of the second

    write(*,'(*(g0))')YR,dash,MO,dash,DY,'T', &
    & HR,colon,MIN,colon,SEC,dot,MILLI
   end associate

   string='no comment allowed &
      &on the end of a continued string &
      ! keep going ...
      & but comment lines are allowed between ' ! but can go on the end

   ! the next exclamation is part of a literal string, and so has
   ! nothing to do with comments
   print *, 'Hello World! X=',x,'Y=',y

end program demo_comment
```
Results:

   > 2024-10-13T0:7:25.283
   > Hello World! X=   4.59107416E-41 Y=   2.76724564E-36

### **See also**

[**continuation**(5)](#continuation),
