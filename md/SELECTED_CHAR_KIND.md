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

  **selected_char_kind(name)** returns the kind value for the character
  set named NAME, if a character set with such a name is supported, or
  **-1** otherwise.

### **Options**

- **name**
  : A name to query the processor kind value of , and/or to determine
  if it is supported. **name** is interpreted without respect to case
  or trailing blanks.

  Currently, supported character sets include "ASCII"
  and "DEFAULT" (iwhich are equivalent), and "ISO_10646" (Universal
  Character Set, UCS-4) which is commonly known as "Unicode".

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
integer, parameter :: ascii = selected_char_kind ("ascii")
integer, parameter :: ucs4  = selected_char_kind ('ISO_10646')

character(kind=ascii, len=26) :: alphabet
character(kind=ucs4,  len=30) :: hello_world

   alphabet = ascii_"abcdefghijklmnopqrstuvwxyz"
   hello_world = ucs4_'Hello World and Ni Hao -- ' &
                 // char (int (z'4F60'), ucs4)     &
                 // char (int (z'597D'), ucs4)

   write (*,*) alphabet

   open (output_unit, encoding='UTF-8')
   write (*,*) trim (hello_world)
end program demo_selected_char_kind
```
Results:

```text
    abcdefghijklmnopqrstuvwxyz
    Hello World and Ni Hao --
```
### **Standard**

Fortran 2003 and later

 _fortran-lang intrinsic descriptions_
