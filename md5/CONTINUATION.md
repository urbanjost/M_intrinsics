## continuation

### **Name**
   CONTINUATION(5) - [FORTRAN] - the rules for free-format line continuation

### **Synopsis**
 general rule:
```fortran
   original long statement

      becomes

   original&
   & long&
   & statement
```
### **Description**

You may split almost all free-format Fortran statements into multiple
lines by inserting the sequence "&\n&", where "\n" represents a
newline. That is, split the line into two lines and place an ampersand
at the right end of the first line and as the first (non-space) character
in the second line.

You __cannot__ split a comment or an **INCLUDE** pre-processor directive
onto multiple lines using this syntax.

The rule for commenting continued lines is simple, really. Comments can
go in the same places on continued lines as on non-continued lines accept
they cannot appear after the right-hand "&" when continuing a long string.

So applying the general rule the line
```fortran
  integer,save :: xx(2,3)= reshape([ 1,2,3,4,5,6 ],shape(xx),order=[2,1])
```
may be split into many lines by simple repeated application of the rule:
```fortran
    integer,save :: xx(2,3)= reshape([&
    & 1, 2, 3,  &
    & 4, 5, 6   &
    &],shape(xx),order=[2,1])
```
Of course, when used for continuation the "&" is not part of the
equivalent concatenated statement.

That is basically it for the general rule, but there are a few variants
and details to cover.

When a line is split using the general rule any trailing spaces before
the amersand at the end of the line are included in the
equivalent single-line statement.

However, spaces before the ampersand beginning the second line are
ignored. So you can indent the lines beginning with an ampersand any
way you like:
```fortran
    integer,save :: xx(2,3)= reshape([&
         & 1, 2, 3, &
         & 4, 5, 6  &
    &],shape(xx),order=[2,1])
```
Now it ends up the leading ampersand is actually optional if not splitting
a lexical token or constant numeric or string value (which is generally
not recommended anyway). If not present the result is the same as if
an ampersand were inserted as the first character of the line -- so the
leading spaces are significant when the leading ampersand is absent. When
not splitting strings multiple spaces generally are treated the same as
a single space so this is equivalent to the previous example:
```fortran
    integer,save :: xx(2,3)= reshape([ &
           1, 2, 3,                    &
           4, 5, 6                     &
    ],shape(xx),order=[2,1])
```
 **COMMENTS ON COMMENTING CONTINUED LINES**

First, note you cannot continue a comment onto another line.  An "&" in
a comment is treated like any other character, with no special effect
(Just start an additional comment line if you want a comment to appear
across multiple lines).

That being said, comments themselves may occur as individual lines inbetween
sections of a continued statement, or after the ending ampersand
__if not continuing a string constant__.

So lets add an explanation about the continued line using in-line comments,
comment lines and blank lines:
```fortran
    integer,save :: xx(2,3)= reshape([& ! define array in row-column order

       !===========!
       & 1, 2, 3,  &   ! row 1
       & 4, 5, 6   &   ! row 2
       !===========!

    ],shape(xx),order=[2,1])
```
So trailing comments are allowed on non-character continuations, and
comment lines and blank lines are always allowed.

Note no line shall contain a single "&" as the only nonblank character
or as the only nonblank character before an ! that initiates a comment.

you have to have the leading amersand on continued lines when splitting
quoted strings or lexical words or constant values.

But try to never split constants or lexical words!

```fortran
    character(len=*), parameter :: str1='my first str', str2='my second str'
```
could be written as
```fortran
    char&
    &acter(len=*), para&
    &meter :: str1='my fi&
    &rst str', str2='my se&
    &cond str'
```
where things were split in two in a haphazard way as long as no spaces
are introduced before the ending amersand and after the leading amersand
that would make the statement illegal if all appearing on one line
(ignoring length for the moment).

This is a more realistic example (a very long string):
```fortran
   character(len=*),parameter='this is a really long string &
     &that I needed to put onto several lines because it would be&
     & so long if I left it on a single line that it might be longer&
     & than allowed in older compilers and would certainly not fit &
     &in my favorite 80-column&
     & terminal window'
```
### **How long you can continue**

Since we are talking about very long lines, how long can a single
statement be? In the Fortran 95 standard, only a maximum of 39
continuation lines is required to be conformant. In Fortran 2003 and
Fortran 2008, at least 255 is to be allowed. There is no limit specified
in Fortran 2018. See your compiler documentation to see if your compiler
still has a limit, but it is probably at least a few hundred lines.

### **Fixed format and include files**

NOTE:
Skip this session if you do not need to deal with (typically old)
fixed-format Fortran files.

Fixed-format Fortran has a very different continuation rule where the
first line has nothing added to it except an optional zero in column
six and all continuations have a non-space non-zero character that is
part of the Fortran character set in column six. If a quoted string is
broken the first line acts as if padded with spaces out to column 72.

Even though the rules for continueing statements on multiple lines are
so different, source code can be formatted in a format that works in
both free and fixed-format files.

Other than being just a curiosity, this is useful if an INCLUDE file is
needed by both free and fixed-format files.  (Note that INCLUDE statements
themselves are one of the few statements that cannot be split across
multiple lines!).

So here is how to make an INCLUDE file for both fixed and free-format files:

+ Conﬁne statement labels to character positions 1 to 5 and statements
  to character positions 7 to 72, which is a requirement of fixed-format.
+ Treat blanks as being significant, which they are in free-format.
+ Use only the exclamation mark (!) to indicate a comment, but do not
  start the comment in character position 6.
+ For continued statements, place an ampersand (&) in both character
  position 73 of a continued line and character position 6 of a
  continuation line.

Why does this work?

If every line being continued has an ampersand in column 73 or further
the ampersand will be ignored by standard fixed-format Fortran.

Combined with the second ampersand always present and in column six
for all but the first line both rules for free and fixed source files
are satisfied.

Fixed-format can use most printable characters in column 6 to indication
continuation. One of the allowed characters is "&", which is the one and
only character used by free-format. So using it obeys both rules.

Therefore the following is equivalent in fixed and free-format parsing:
```fortran
 >12345 continue
 >      character(len=*), parameter :: string1="hello world", string2="hel&
 >     &lo world"
```
Obviously, this is not compatible with extended length fixed-format
source files (which some compilers support as an extension) unless
the ampersand is shifted beyond the extended limit (which in standard
fixed-format files would be past column 72).

You may want to look for a compiler option to disable long-line warnings
when using characters past column 72.

### **Example**
Example program
```fortran
program demo_continuation
implicit none
integer :: point(3)
character(len=:),allocatable :: string

! one statement using continuation:
integer,save :: xx(3,5)= reshape([& ! define in row-column order
!-------------------------!
 1,    2,   3,   4,   5,  &  ! row 1
 10,  20,  30,  40,  50,  &  ! row 2
 11,  22,  33,  44,  55   &  ! row 3
!-------------------------!

],shape(xx),order=[2,1])

! print it in row-column order too
  call print_matrix_int('xx array:',xx)
  xx(3,5)= -1051
  call print_matrix_int('xx array:',xx)

! So this is OK:
   POINT=[&   ! define a Point <X,Y,Z>
   & 10, &    ! the X component
   & 20, &    ! the Y component
   & 30  ]    ! the Z component

! because you can have comments after the ampersand when it is not
! a string.
! But this is not OK:
!   STRING='&    ! create a sentence
!   & This&      ! first word
!   & is&        ! second word
!   & sentence&  ! third word
!   & a'         ! forth word (a comment here is OK)
!Because when continuing a string you cannot have a comment after the "&".
!
! This is OK:
   STRING='&
   ! create a sentence
   & This&
   ! first word
   & is&
   ! second word
   & sentence&
   ! third word
   & a'        ! forth word (a comment here is OK)
! because comment LINES can go anywhere in Fortran source files

! Dusty corners
   call splitting_a_token()
   call longstring()
contains

subroutine splitting_a_token()

! Often denoted by "e" in honor of Euler,
! Napier's constant is the base of the natural logarithm system.
real(kind=kind(0.0d0)),parameter :: &
& Napier_constant = 2.71828182845904523d0

! without continuation
write(*,*)napier_constant

! splitting a token the & is required
write(*,*)napier_&
&constant

! the left-hand ampersand is required when splitting constants to,
! including characters strings
write(*,*)'Expecting &
          &the value',2.71828182&
          &845904523d0

!NOT ALLOWED <<<<<<
!write(*,*)napier_&
!constant
!>>>>>>>

! splitting a token is not recommended as it complicates identifying
! the use of a token name.

end subroutine splitting_a_token
Subroutine LongString()
! Long strings:

Character (len=200) :: string1, String2
character(len=:), allocatable :: a,b,c, big

   string1 = "A very long string that won't fit on a single &
              &line can be made through proper continuation."

   ! alternatives to continuation lines
   string2 = "A very long string that won't fit on a single " // &
             "line can be made through proper continuation " // &
             "and concatenation of multiple strings."
   print *, "string1=",string1
   print *, "string2=",string2

   ! append multiple strings together to construct a long line
   a=repeat('A',100)
   b=repeat('B',100)
   big=a//b
   c=repeat('C',100)
   big=a//c
   big=big//"more at end"
   print *, "big=",big

End Subroutine LongString

subroutine print_matrix_int(title,arr)
! bonus points -- print an integer array in RC order with bells on.
! ie. It calculates the width needed for the longest variable and
! puts a frame around the array
implicit none
character(len=*),intent(in)  :: title
integer,intent(in)           :: arr(:,:)
integer                      :: i
integer                      :: size_needed
character(len=:),allocatable :: biggest
  write(*,*)trim(title)
  biggest='           '  ! make buffer to write integer into
  ! find how many characters to use for integers
  size_needed=ceiling(log10(real(maxval(abs(arr)))))+2
  write(biggest,'(i0)')size_needed
  ! use this format to write a row
  biggest='("   |",*(i'//trim(biggest)//':," |"))'
  ! print one row of array at a time
  write(*,'(*(g0))')&
  &'   #',(repeat('-',size_needed),'-#',i=1,size(arr,dim=2))
  do i=1,size(arr,dim=1)
     write(*,fmt=biggest,advance='no')arr(i,:)
     write(*,'(" |")')
  enddo
  write(*,'(*(g0))')&
  &'   #',(repeat('-',size_needed),'-#',i=1,size(arr,dim=2))
end subroutine print_matrix_int
end program demo_continuation
``
  Results:
```text
 xx array:
   #-----#-----#-----#-----#-----#
   |   1 |   2 |   3 |   4 |   5 |
   |  10 |  20 |  30 |  40 |  50 |
   |  11 |  22 |  33 |  44 |  55 |
   #-----#-----#-----#-----#-----#
 xx array:
   #-------#-------#-------#-------#-------#
   |     1 |     2 |     3 |     4 |     5 |
   |    10 |    20 |    30 |    40 |    50 |
   |    11 |    22 |    33 |    44 | -1051 |
   #-------#-------#-------#-------#-------#
   2.7182818284590451
   2.7182818284590451
 Expecting the value   2.7182818284590451
 string1=A very long string that won't fit on a single \
 line can be made through proper continuation.
 string2=A very long string that won't fit on a single \
 line can be made through proper continuation and \
 concatenation of multiple strings.
 big=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\
 AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACCCCCC\
 CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC\
 CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCmore at end
```
 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
