## previous

### **Name**

**previous**(3) - \[ENUMERATION\] Previous enumeration value

### **Synopsis**
```fortran
    result = previous (a [, stat])
```
```fortran
     elemental enumerator function previous(a,stat) result(answer)

      enumerator,intent(in) :: a
      integer(kind=**),intent(out),optional :: stat
      enumerator :: answer
```
### **Characteristics**

 - **A** shall be of enumeration type.
 - **STAT** is an integer with a decimal exponent range of at least four.
 - The returned value will be of the same type and kind as the argument.
   If **A** is equal to the first enumerator of its type, it is assigned

### **Description**
   Previous enumeration value

### **Options**

- **a**
  : the starting value to locate the previous value relative to

- **stat**
  : If **A** is equal to the last enumerator of its type, it is assigned a
    processor-dependent positive value; otherwise, it is assigned the
    value zero. If STAT would have been  assigned a nonzero value
    but is not present, error termination is  initiated.

### **Result**
   If **A** is equal to the first enumerator of its type,
   the value of the result is that of **a**. Otherwise, the value of
   the result is the enumerator preceding the value of **A**.

### **Example**

   Example. If the enumerators of an enumeration type are EN1, EN2,
   EN3, and EN4, PREVIOUS (EN3) is equal to EN2, and PREVIOUS (EN1,
   ISTAT) is equal to EN1 and a positive value is assigned to ISTAT.

Sample program:

```fortran
program demo_previous
implicit none

! Fortran 2023 strongly-typed enumeration
enum, bind(c) :: color
   enumerator :: red, green, blue
end enum

type(color) :: current_color

  ! Initialize to the first item
  current_color = red
  print *, "Initial position: ", int(current_color)

  ! Advance using the new NEXT intrinsic
  current_color = next(current_color)
  print *, "Next position (green): ", int(current_color)

  ! Advance again
  current_color = next(current_color)
  print *, "Next position (blue): ", int(current_color)

  ! Move backward using the new PREVIOUS intrinsic
  current_color = previous(current_color)
  print *, "Previous position (green): ", int(current_color)

end program demo_previous
```
Results:
```text
 >
 >
```
### **Standard**

Fortran 2023

### **See Also**
 - Next enumeration value: [**next**(3)](next)
 - Conversion of position to INTEGER: [**int**(3)](int)

 _Fortran intrinsic descriptions_
