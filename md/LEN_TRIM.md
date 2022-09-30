## len_trim

### **Name**

**len_trim**(3) - \[CHARACTER:WHITESPACE\] Character length without trailing blank characters

### **Synopsis**
```fortran
  result = len_trim(string [,kind])
```
```fortran
   elemental integer(kind=KIND) function len_trim(string,kind)

    character(len=*),intent(in) :: string
    integer(kind=KIND),intent(in),optional :: kind
```
### **Characteristics**

**string** is a scalar or array of type _character_

**kind** is a scalar integer constant expression specifying the kind
of the returned value.

The return value is of type _integer_ and of kind **kind**. If **kind**
is absent, the return value is of default _integer_ kind.

### **Description**

Returns the length of a character string, ignoring any trailing blanks.

### **Options**

- **string**
  : The input string whose length is to be measured.

- **kind**
  : Indicates the kind parameter of the result.

### **Result**

  The result has a value equal to the number of characters remaining
  after any trailing blanks in STRING are removed. If the argument
  contains no nonblank characters, the result is zero.

### **Examples**

Sample program
```fortran
program demo_len_trim
implicit none
character(len=:),allocatable :: string
integer :: i
! basic usage
   string=" how long is this string?     "
   write(*,*) string
   write(*,*)'UNTRIMMED LENGTH=',len(string)
   write(*,*)'TRIMMED LENGTH=',len_trim(string)

   ! print string, then print substring of string
   string='xxxxx   '
   write(*,*)string,string,string
   i=len_trim(string)
   write(*,*)string(:i),string(:i),string(:i)
   !
  ! elemental example
   ELE:block 
   ! an array of strings may be used
   character(len=:),allocatable :: tablet(:)
   tablet=[character(len=256) :: &
   & ' how long is this string?     ',&
   & 'and this one?']
      write(*,*)'UNTRIMMED LENGTH=  ',len(tablet)
      write(*,*)'TRIMMED LENGTH=    ',len_trim(tablet)
      write(*,*)'SUM TRIMMED LENGTH=',sum(len_trim(tablet))
   endblock ELE
   !
end program demo_len_trim
```
  Results:
```text
     how long is this string?     
    UNTRIMMED LENGTH=          30
    TRIMMED LENGTH=          25
    xxxxx   xxxxx   xxxxx   
    xxxxxxxxxxxxxxx
    UNTRIMMED LENGTH=           256
    TRIMMED LENGTH=              25          13
    SUM TRIMMED LENGTH=          38
```
### **Standard**

Fortran 95 and later. **kind** argument added with Fortran 2003.

### **See Also**

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](#adjustl),
  [**adjustr**(3)](#adjustr),
  [**index**(3)](#index),
  [**scan**(3)](#scan),
  [**verify**(3)](#verify)

- **Nonelemental:**
  [**repeat**(3)](#repeat),
  [**len**(3)](#len),
  [**trim**(3)](#trim)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
