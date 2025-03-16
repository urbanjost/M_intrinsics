## split

### **Name**

**split**(3) - \[CHARACTER:PARSE\] Parse a string into tokens, one at a time

### **Synopsis**
```fortran
     call split (string, set, pos [, back])

      character(kind=KIND),intent(in)       :: string
      character(len=*,kind=KIND),intent(in) :: set
      integer,intent(inout)                 :: pos
      logical,intent(in),optional           :: back
```
### **Characteristics**
- **string** is a scalar character variable
- **set** is a scalar character variable of the same kind as **string**.

### **Description**

  Find the extent of consecutive tokens in a string. Given a string and
  a position to start looking for a token return the position of the
  end of the token. A set of separator characters may be specified as
  well as the direction of parsing.

  Typically consecutive calls are used to parse a string into a set of
  tokens by stepping through the start and end positions of each token.

### **Options**

 - **string**
   : The string to search for tokens in.

 - **set**
   : Each character in **set** is a token delimiter. A sequence of zero or
     more characters in **string** delimited by any token delimiter,
     or the beginning or end of **string**, comprise a token. Thus, two
     consecutive token delimiters in **string**, or a token delimiter
     in the first or last character of **string**, indicate a token with
     zero length.

 - **pos**
   : On input, the position from which to start looking for the next
     separator from. This is typically the first character or the
     last returned value of **pos** if searching from left to right (ie.
     **back** is absent or _.true._) or the last character or the last
     returned value of **pos** when searching from right to left (ie.
     when **back** is _.false._).

     If **back** is present with the value _.true._, the value of **pos**
     shall be in the range 0 < POS <= LEN(STRING)+1; otherwise it shall
     be in the range 0 <= POS <= LEN(STRING).

     So **pos** on input is typically an end of the string or the
     position of a separator, probably from a previous call to **split**
     but **pos** on input can be any position in the range 1 <= POS <=
     LEN(STRING).  If **pos** points to a non-separator character in the
     string the call is still valid but it will start searching from
     the specified position and that will result (somewhat obviously)
     in the string from **pos** on input to the returned **pos** being
     a partial token.

 - **back**
   : If **back** is absent or is present with the value _.false._,
     **pos** is assigned the position of the leftmost token delimiter in
     **string** whose position is greater than **pos**, or if there is no
     such character, it is assigned a value one greater than the length
     of **string**. This identifies a token with starting position one
     greater than the value of **pos** on invocation, and ending position
     one less than the value of **pos** on return.

     If **back** is present with the value _.true._, **pos** is assigned
     the position of the rightmost token delimiter in **string** whose
     position is less than **pos**, or if there is no such character,
     it is assigned the value zero. This identifies a token with ending
     position one less than the value of **pos** on invocation, and
     starting position one greater than the value of **pos** on return.

### **Example**
Sample program:
```fortran
program demo_split
   !use m_strings, only: split=>split2020
   implicit none
   character (len=:), allocatable :: input
   integer :: position, istart, iend
   input = "one,last example,,x,, ,,"
   position = 0
   ! write a number line
   write(*,'(t3,a)') repeat('1234567890',6)
   ! display the input line
   write(*,'(t3,a)') input
   ! step through the input string locating the bounds of the
   ! next token and printing it
   do while (position < len(input))
      istart = position + 1
      call split (input, set=', ', pos=position)
      iend = position - 1
      if(iend >= istart)then
         print '(t3,a,1x,i0,1x,i0)', input (istart:iend),istart,iend
      else
         ! maybe ignore null fields, maybe not ...
         write(*,'(t3,*(g0))')'null between ',iend,' and ',istart
      endif
   end do
end program demo_split
```
Results:
```text
 >   123456789012345678901234567890123456789012345678901234567890
 >   one,last example,,x,, ,,
 >   one 1 3
 >   last 5 8
 >   example 10 16
 >   null between 17 and 18
 >   x 19 19
 >   null between 20 and 21
 >   null between 21 and 22
 >   null between 22 and 23
 >   null between 23 and 24
```
### **Standard**

Fortran 2023

### **See Also**

  - [**tokenize**(3)](#tokenize) - Parse a string into tokens
  - [**index**(3)](#index) - Position of a substring within a string
  - [**scan**(3)](#scan) - Scan a string for the presence of a set
    of characters
  - [**verify**(3)](#verify) - Position of a character in a string of
    characters that does not appear in a given set of characters.

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
