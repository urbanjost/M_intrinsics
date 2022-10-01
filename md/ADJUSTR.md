## adjustr

### **Name**

**adjustr**(3) - \[CHARACTER:WHITESPACE\] Right-adjust a string

### **Synopsis**
```fortran
    result = adjustr(string)
```
```fortran
     elemental character(len=len(string)) function adjustr(string)

      character(len=*),intent(in) :: string
```
### **Characteristics**
- **string** is a _character_ variable
- The return value is a _character_ variable of the same kind as
  **string**

### **Description**

**adjustr(3f)** right-adjusts a string by removing trailing spaces. Spaces
are inserted at the start of the string as needed to retain the original
length.

### **Options**

- **string**
  : the string to right-adjust

### **Result**

Trailing spaces are removed and the same number of spaces are inserted
at the start of **string**.

### **Examples**

Sample program:

```fortran
program demo_adjustr
implicit none
character(len=20) :: str 
   ! print a short number line
   write(*,'(a)')repeat('1234567890',2)

  ! basic usage
   str = '  sample string '
   write(*,'(a)') str
   str = adjustr(str)
   write(*,'(a)') str

   !
   ! elemental
   !
   write(*,'(a)')repeat('1234567890',5)
   write(*,'(a)')adjustr([character(len=50) :: &
   '  first           ', &
   '     second       ', &
   '         third    ' ])
   write(*,'(a)')repeat('1234567890',5)

end program demo_adjustr
```
Results:
```text
   12345678901234567890
     sample string      
          sample string
   12345678901234567890123456789012345678901234567890
                                                first
                                               second
                                                third
   12345678901234567890123456789012345678901234567890
```
### **Standard**

Fortran 95

### **See Also**

[**adjustl**(3)](#adjustl),
[**trim**(3)](#trim)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
