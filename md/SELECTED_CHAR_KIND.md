## selected_char_kind

### **Name**

**selected_char_kind**(3) - \[KIND\] Select character kind such as "Unicode"

### **Synopsis**
```fortran
    result = selected_char_kind(name)
```
```fortran
     integer function selected_char_kind(name)

      character(len=*),intent(in) :: name
```
### **Characteristics**

### **Description**

  **selected_char_kind**(3) returns the kind value for the character
  set named NAME, if a character set with such a name is supported, or
  **-1** otherwise.

### **Options**

- **name**
  : A name to query the processor kind value of , and/or to determine
  if it is supported. **name** is interpreted without respect to case
  or trailing blanks.

  Currently, supported character sets include "ASCII" and "DEFAULT" and
  "ISO_10646" (Universal Character Set, UCS-4) which is commonly known as
  "Unicode". Supported names other than "DEFAULT" are processor dependent.

### **Result**

If a name is not supported, -1 is returned. Otherwise

 + If NAME has the value "DEFAULT", then the result has a value equal to
   that of the kind type parameter of default character. This name is
   always supported.

 + If NAME has the value "ASCII", then the result has a value equal
   to that of the kind type parameter of ASCII character.

 + If NAME has the value "ISO_10646", then the result has a value equal
   to that of the kind type parameter of the ISO 10646 character kind
   (corresponding to UCS-4 as specified in ISO/IEC 10646).

 + If NAME is a processor-defined name of some other character kind
   supported by the processor, then the result has a value equal to that
   kind type parameter value.

### **Examples**

Sample program:

```fortran
program demo_selected_char_kind
use iso_fortran_env
implicit none

intrinsic date_and_time,selected_char_kind

integer, parameter :: default = selected_char_kind ("default")
integer, parameter :: ascii =   selected_char_kind ("ascii")
integer, parameter :: ucs4  =   selected_char_kind ('ISO_10646')
integer, parameter :: utf8  =   selected_char_kind ('utf-8')

character(len=26, kind=ascii ) :: alphabet
character(len=30, kind=ucs4  ) :: hello_world
character(len=30, kind=ucs4  ) :: string

   write(*,*)'ASCII     ',&
    & merge('SUPPORTED    ','NOT SUPPORTED',ascii /= -1) 
   write(*,*)'ISO_10646 ',&
    & merge('SUPPORTED    ','NOT SUPPORTED',ucs4 /= -1)
   write(*,*)'UTF-8     ',&
    & merge('SUPPORTED    ','NOT SUPPORTED',utf8 /= -1)

   if(default.eq.ascii)then
       write(*,*)'ASCII is the default on this processor'
   endif

  ! the kind precedes the value, somewhat like a BOZ constant
   alphabet = ascii_"abcdefghijklmnopqrstuvwxyz"
   write (*,*) alphabet

   hello_world = ucs4_'Hello World and Ni Hao -- ' &
                 // char (int (z'4F60'), ucs4)     &
                 // char (int (z'597D'), ucs4)

  ! an  encoding option is required on OPEN for non-default I/O
   if(ucs4 /= -1 )then
      open (output_unit, encoding='UTF-8')
      write (*,*) trim (hello_world)
   else
      write (*,*) 'cannot use utf-8'
   endif

   call create_date_string(string)
   write (*,*) trim (string)

contains

! The following produces a Japanese date stamp.
subroutine create_date_string(string)
intrinsic date_and_time,selected_char_kind
integer,parameter :: ucs4 = selected_char_kind("ISO_10646")
character(len=1,kind=ucs4),parameter :: &
       nen =   char(int( z'5e74' ),ucs4), & ! year
       gatsu = char(int( z'6708' ),ucs4), & ! month
       nichi = char(int( z'65e5' ),ucs4)    ! day
     character(len= *, kind= ucs4) string
     integer values(8)
     call date_and_time(values=values)
     write(string,1) values(1),nen,values(2),gatsu,values(3),nichi
   1 format(i0,a,i0,a,i0,a)
end subroutine create_date_string

end program demo_selected_char_kind
```
Results:

The results are very processor-dependent
```text
 ASCII     SUPPORTED    
 ISO_10646 SUPPORTED    
 UTF-8     NOT SUPPORTED
 ASCII is the default on this processor
 abcdefghijklmnopqrstuvwxyz
 Hello World and Ni Hao -- 你好
 2022年10月5日
```
### **Standard**

Fortran 2003

### **See also**

[**achar**(3)](#achar)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
