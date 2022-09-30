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

### **Description**

**adjustr(string)** right-adjusts a string by removing trailing
spaces. Spaces are inserted at the start of the string as needed to
retain the original length.

### **Options**

- **string**
  : the type shall be _character_.

### **Result**

The return value is of type _character_ and of the same kind as **string**
where trailing spaces are removed and the same number of spaces are
inserted at the start of **string**.

### **Examples**

Sample program:

```fortran
program demo_adjustr
implicit none
character(len=20) :: str = ' sample string '
   ! print a short number line
   write(*,'(a)')repeat('1234567890',5)

   !
   ! basic usage
   !
   str = adjustr(str)
   write(*,'(a)') str

   !
   ! elemental
   !
   write(*,'(a)')adjustr([character(len=50) :: &
   '  first           ', &
   '     second       ', &
   '         third    ' ])

   write(*,'(a)')repeat('1234567890',5)
end program demo_adjustr
```

Results:

```text
   12345678901234567890123456789012345678901234567890
          sample string
                                                first
                                               second
                                                third
   12345678901234567890123456789012345678901234567890
```

### **Standard**

Fortran 95 and later

### **See Also**

[**adjustl**(3)](#adjustl)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
