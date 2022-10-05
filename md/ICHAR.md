## ichar

### **Name**

**ichar**(3) - \[CHARACTER:CONVERSION\] Character-to-integer conversion function

### **Synopsis**
```fortran
    result = ichar(c [,kind])
```
```fortran
     elemental function ichar(c,kind)

      character(len=1),intent(in) :: c
      integer,intent(in),optional :: kind
```
### **Characteristics**

- **c** is a single scalar _character_
- **kind** a constant _integer_ initialization expression indicating
  the kind parameter of the result.
- The return value is of type _integer_ and of kind **kind**. If **kind**
  is absent, the return value is of default _integer_ kind.

### **Description**

 **ichar**(3) returns the code for the character in the system's native
 character set. The correspondence between characters and their codes is
 not necessarily the same across different Fortran implementations. For
 example, a platform using EBCDIC would return different values than an
 ASCII platform.

 See **iachar**(3) for specifically working with the ASCII character set.

### **Options**

- **c**
  : The input character to determine the code for.

- **kind**
  : indicates the kind parameter of the result.  If **kind** is absent,
  the return value is of default _integer_ kind.

### **Result**

The code in the systems default character set for the character being queried.

### **Examples**

Sample program:

```fortran
program demo_ichar
implicit none
integer i

   write(*,*)ichar(['a','z','A','Z'])

end program demo_ichar
```
Results:
```text
             97         122          65          90
```
### **Standard**

Fortran 95 , with KIND argument -Fortran 2003

### **See Also**

[**achar**(3)](#achar),
[**char**(3)](#char),
[**iachar**(3)](#iachar)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](#adjustl),
  [**adjustr**(3)](#adjustr),
  [**index**(3)](#index),

[**scan**(3)](#scan),
[**verify**(3)](#verify)

- **Nonelemental:**
  [**len_trim**(3)](#len_trim),
  [**len**(3)](#len),
  [**repeat**(3)](#repeat),
  [**trim**(3)](#trim)

 _fortran-lang intrinsic descriptions_
