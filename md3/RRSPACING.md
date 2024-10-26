## rrspacing

### **Name**

**rrspacing**(3) - \[MODEL_COMPONENTS\] Reciprocal of the relative
spacing of a numeric type

### **Synopsis**
```fortran
    result = rrspacing(x)
```
```fortran
     elemental real(kind=KIND) function rrspacing(x)

      real(kind=KIND),intent(in) :: x
```
### **Characteristics**

 - **x** is type _real_ of any kind
 - The return value is of the same type and kind as **x**.

### **Description**

**rrspacing**(3) returns the reciprocal of the relative spacing of model
numbers near **x**.

<!--
 5 Result Value. The result has the value
   |Y* b-e|*bp = ABS (FRACTION (Y)) * RADIX (X) / EPSILON (X),
   where b, e, and p are as defined in 16.4 for Y, the value nearest
   to X in the model for real values whose kind type
   parameter is that of X; if there are two such values, the value of
   greater absolute value is taken. If X is an IEEE
   infinity, the result is an IEEE NaN. If X is an IEEE NaN, the result
   is that NaN.
-->

### **Options**

- **x**
  : Shall be of type _real_.

### **Result**

  The return value is of the same type and kind as **x**. The value
  returned is equal to
  **abs(fraction(x)) \* float(radix(x))\*\*digits(x)**.

### **Examples**

Sample program:

```fortran
program demo_rrspacing
implicit none
integer, parameter :: sgl = selected_real_kind(p=6, r=37)
integer, parameter :: dbl = selected_real_kind(p=13, r=200)
character(len=*),parameter :: gen='(*(g0))', nl=new_line('A')
real(kind=sgl) :: x
   x=-3.0_sgl
   print gen, &
   'rrspacing(',x,'_sgl)=', rrspacing(x),                       nl, &
   'rrspacing(x)=abs(fraction(x))*float(radix(x))**digits(x)',  nl, &
   'so this should be the same as rrspacing():',                nl, &
   abs( fraction(x) ) * float( radix(x) )**digits(x),           nl, &
   'RRSPACING (-3.0) has the value 0.75x2**24 for reals',       nl, &
   'on current typical platforms. For reference:',              nl, &
   '   0.75*2**24=', 0.75*2**24,                                nl, &
   'sign should not matter, so',rrspacing(x)==rrspacing(-x),    nl, &
   'note the kind of the value is significant',                 nl, &
   rrspacing(-3.0_dbl),                                         nl, &
   'for common platforms rrspacing(487923.3d0)=>',              nl, &
   '   8.382458680573952E+015',                                 nl, &
   rrspacing(487923.3d0),                                       nl, &
   ' '
end program demo_rrspacing
```
```text
 > rrspacing(-3.00000000_sgl)=12582912.0
 > rrspacing(x)=abs(fraction(x))*float(radix(x))**digits(x)
 > so this should be the same as rrspacing():
 > 12582912.0
 > RRSPACING (-3.0) has the value 0.75x2**24 for reals
 > on current typical platforms. For reference:
 > 0.75*2**24=12582912.0
 > sign should not matter, soT
 > note the kind of the value is significant
 > 6755399441055744.0
 > for common platforms rrspacing(487923.3d0)=>8.382458680573952E+015
 > 8382458465825587.0
```
### **Standard**

Fortran 90

### **See Also**

[**digits**(3)](#digits),
[**epsilon**(3)](#epsilon),
[**exponent**(3)](#exponent),
[**fraction**(3)](#fraction),
[**huge**(3)](#huge),
[**maxexponent**(3)](#maxexponent),
[**minexponent**(3)](#minexponent),
[**nearest**(3)](#nearest),
[**precision**(3)](#precision),
[**radix**(3)](#radix),
[**range**(3)](#range),
[**scale**(3)](#scale),
[**set_exponent**(3)](#set_exponent),
[**spacing**(3)](#spacing),
[**tiny**(3)](#tiny)

 _Fortran intrinsic descriptions_
