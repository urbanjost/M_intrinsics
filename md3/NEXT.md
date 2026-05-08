## next

### **Name**

**next**(3) - \[ENUMERATION\] Next enumeration value

### **Synopsis**
```fortran
    result = next (a [, stat])
```
```fortran
     elemental enumerator function next(a,stat) result(answer)

      enumerator,intent(in) :: a
      integer(kind=**),intent(out),optional :: stat
      enumerator :: answer
```
### **Characteristics**

 - **A** shall be of enumeration type.
 - **STAT** is an integer with a decimal exponent range of at least four.
 - The returned value will be of the same type and kind as the argument.

### **Description**
   Next enumeration value

### **Options**

- **a**
  : The starting value to use to locate the next value from

- **stat**
  : If **a** is equal to the last enumerator of its type, it is assigned a
    processor-dependent positive value; otherwise, it is assigned the
    value zero. If STAT would have been  assigned a nonzero value
    but is not present, error termination is  initiated.

### **Result**

   If **a** is equal to the last enumerator of its type,
   the value of the result is that of A. Otherwise, the value of the
   result is the next enumerator following the value of A.

   For example, if the enumerators of an enumeration type are EN1, EN2,
   EN3, and EN4, NEXT (EN1) is equal to EN2, and NEXT (EN4, ISTAT)
   is equal to EN4 and a positive value is assigned to ISTAT.

Sample program:

```fortran
!program demo_next
module enumeration_mod

enumeration type :: v_value
   enumerator :: v_one, v_two, v_three
   enumerator v_four
end enumeration type

enumeration type :: w_value
   enumerator :: w1, w2, w3, w4, w5, w_endsentinel
end enumeration type

contains

subroutine sub(a)
type(v_value),intent(in) :: a
   print 1,a ! Acts similarly to Print *,Int(a).
1  format('A has ordinal value ',i0)
end subroutine

subroutine wcheck(w)
type(w_value),intent(in) :: w
   select case(w)
    case(w1)
      print *,'w1 selected'
    case (w2:w4)
      print *,'One of w2...w4 selected'
    case (w_endsentinel)
      stop 'Invalid w selected'
    case default
      stop 'Unrecognized w selected'
   end select
end subroutine

end module
program demo_next
! Here is an example of a program using that module.
use enumeration_mod
type(v_value) :: x = v_one
type(v_value) :: y = v_value(2)  ! Explicit constructor producing v_two.
type(v_value) :: z,nz            ! Initially undefined.
   call sub(x)
   call sub(v_three)
   z = v_value(1)                ! First value.
   do
      if (z==huge(x)) write (*,'(A)',advance='No') ' Huge:'
      call sub(z)
      nz = next(z)
      if (z==nz) exit
      z = nz
   end do

end program demo_next
```
Results:
```text
 >
 >
```
Here is an example showing some invalid usages of enumerations.

Program invalid
Use enumeration_mod
```fortran
Type(v_value) :: a, b
   a = 1         ! INVALID - wrong type (INTEGER).
   b = w1        ! INVALID - wrong enumeration type.
   Print *,a     ! INVALID - list-directed i/o not available.
End Program
```
An enumeration type can be used to declare components, for example:
```fortran
Module example2
Use enumeration_mod
Type vw
   Type(v_value) v
   Type(w_value) w
End Type

Contains
Subroutine showme(ka)
Type(vw),Intent(In) :: ka
   Print 1,ka
1  Format(1X,'v ordinal is ',I0,', w ordinal is ',I0)
End Subroutine
End Module
```
### **Standard**

Fortran 2023

### **See Also**
 - Next enumeration value: [**previous**(3)](previous)
 - Conversion of position to INTEGER: [**int**(3)](int)

 _Fortran intrinsic descriptions_
