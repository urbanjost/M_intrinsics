## tokenize

### **Name**

**tokenize**(3) - \[CHARACTER:PARSE] Parse a string into tokens.

### **Synopsis**
  TOKEN form (returns array of strings)
```fortran
    subroutine tokenize(string, set, tokens [, separator])

     character(len=*),intent(in) :: string
     character(len=*),intent(in) :: set
     character(len=:),allocatable,intent(out) :: tokens(:)
     character(len=1),allocatable,intent(out),optional :: separator(:)
```
  ARRAY BOUNDS form (returns arrays defining token positions)
```fortran
    subroutine tokenize (string, set, first, last)

     character(len=*),intent(in) :: string
     character(len=*),intent(in) :: set
     integer,allocatable,intent(out) :: first(:)
     integer,allocatable,intent(out) :: last(:)
```
### **Characteristics**
  - **string** - a scalar of type character. It is an INTENT(IN) argument.
  - **set** - a scalar of type character with the same kind type parameter
              as **string**. It is an INTENT(IN) argument.
  - **separator** - (optional) shall be of type character with the same
                    kind type parameter as **string**. It is an
                    INTENT(OUT)argument. It shall not be a coarray or
                    a coindexed object.
  - **tokens** - of type character with the same kind type parameter as
                 **string**. It is an INTENT(OUT) argument. It shall
                 not be a coarray or a coindexed object.
  - **first**,**last** - an allocatable array of type integer and rank
                        one. It is an INTENT(OUT) argument. It shall
                        not be a coarray or a coindexed object.

  To reiterate, **string**, **set**, **tokens** and **separator** must
  all be of the same CHARACTER kind type parameter.

### **Description**

   **tokenize(3)** parses a string into tokens. There are two forms
   of the subroutine **tokenize(3)**.

   - The token form returns an array with one token per element, all of
     the same length as the longest token.
   - The array bounds form returns two integer arrays. One contains the
     beginning position of the tokens and the other the end positions.

   Since the token form pads all the tokens to the same length the
   original number of trailing spaces of each token accept for the
   longest is lost.

   The array bounds form retains information regarding the exact token
   length even when padded by spaces.


### **Options**
- **string**
  : The string to parse into tokens.

- **set**
  : Each character in **set** is a token delimiter. A sequence of zero
    or more characters in **string** delimited by any token delimiter,
    or the beginning or end of **string**, comprise a token. Thus, two
    consecutive token delimiters in **string**, or a token delimiter
    in the first or last character of **string**, indicate a token with
    zero length.

- **tokens**
  : It shall be an allocatable array of rank one with deferred length. It
    is allocated with the lower bound equal to one and the upper bound
    equal to the number of tokens in **string**, and with character
    length equal to the length of the longest token.

    The tokens in **string** are assigned in the order found, as if
    by intrinsic assignment, to the elements of **tokens**, in array
    element order.

- **first**
  : shall be an allocatable array of type integer and rank one. It is
    an INTENT(OUT) argument. It shall not be a coarray or a coindexed
    object.

    It is allocated with the lower bound equal to one and the
    upper bound equal to the number of tokens in **string**. Each element
    is assigned, in array element order, the starting position of each
    token in **string**, in the order found. 

    If a token has zero length, the starting position is equal to one
    if the token is at the beginning of **string**, and one greater than
    the position of the preceding delimiter otherwise.

- **last**
  : It is allocated with the lower bound equal to one and the
    upper bound equal to the number of tokens in **string**. Each element
    is assigned, in array element order, the ending position of each
    token in **string**, in the order found. 

    If a token has zero length, the ending position is one less than
    the starting position.

### **Examples**

  Sample of uses
```fortran
    program demo_tokenize
    !use M_strings, only : tokenize=>split2020
    implicit none
    ! some useful formats
    character(len=*),parameter :: brackets='(*("[",g0,"]":,","))'
    character(len=*),parameter :: a_commas='(a,*(g0:,","))'
    character(len=*),parameter :: space='(*(g0:,1x))'
    character(len=*),parameter :: gen='(*(g0))'

    ! Execution of TOKEN form (return array of tokens)

    block
       character (len=:), allocatable :: string
       character (len=:), allocatable :: tokens(:)
       character (len=:), allocatable :: kludge(:)
       integer                        :: i
       string = '  first,second ,third       '
       call tokenize(string, set=';,', tokens=tokens )
       write(*,brackets)tokens

       string = '  first , second ,third       '
       call tokenize(string, set=' ,', tokens=tokens )
       write(*,brackets)(trim(tokens(i)),i=1,size(tokens))
       ! remove blank tokens
       ! <<<
       !tokens=pack(tokens, tokens /= '' )
       ! gfortran 13.1.0 bug -- concatenate //'' and use scratch
       ! variable KLUDGE. JSU: 2024-08-18
       kludge=pack(tokens//'', tokens /= '' )
       ! >>>
       write(*,brackets)kludge

    endblock

    ! Execution of BOUNDS form (return position of tokens)

    block
       character (len=:), allocatable :: string
       character (len=*),parameter :: set = " ,"
       integer, allocatable        :: first(:), last(:)
       write(*,gen)repeat('1234567890',6)
       string = 'first,second,,fourth'
       write(*,gen)string
       call tokenize (string, set, first, last)
       write(*,a_commas)'FIRST=',first
       write(*,a_commas)'LAST=',last
       write(*,a_commas)'HAS LENGTH=',last-first.gt.0
    endblock

    end program demo_tokenize
```
Results:
```text
 > [  first     ],[second      ],[third       ]
 > [],[first],[],[],[second],[],[third],[],[],[],[],[]
 > [first ],[second],[third ]
 > 123456789012345678901234567890123456789012345678901234567890
 > first,second,,fourth
 > FIRST=1,7,14,15
 > LAST=5,12,13,20
 > HAS LENGTH=T,T,F,T
```
### **Standard**

Fortran 2023

### **See Also**

  - [**split**(3)](#split) - return tokens from a string, one at a time
  - [**index**(3)](#index) - Position of a substring within a string
  - [**scan**(3)](#scan) - Scan a string for the presence of a set
    of characters
  - [**verify**(3)](#verify) - Position of a character in a string of
    characters that does not appear in a given set of characters.

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
