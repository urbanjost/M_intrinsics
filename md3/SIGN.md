## sign

### **Name**

**sign**(3) - \[NUMERIC\] Sign copying function

### **Synopsis**
```fortran
    result = sign(a, b)
```
```fortran
     elemental type(TYPE(kind=KIND))function sign(a, b)

      type(TYPE(kind=KIND)),intent(in) :: a, b
```
### **Characteristics**

 - **a** shall be of type integer or real.
 - **b** shall be of the same type as **a**.
 - the characteristics of the result are the same as **a**.

### **Description**

  **sign**(3) returns a value with the magnitude of _a_ but with the
  sign of _b_.

  For processors that distinguish between positive and negative zeros
  _sign()_ may be used to distinguish between _real_ values 0.0 and
  -0.0. SIGN (1.0, -0.0) will return -1.0 when a negative zero is
  distinguishable.

### **Options**

  - **a**
    : The value whose magnitude will be returned.

  - **b**
    : The value whose sign will be returned.

### **Result**

  a value with the magnitude of **a** with the sign of **b**. That is,

  - if the processor distinguishes between
    positive and negative real zero, and _b_ is negative real zero, the
    value of the result is _-abs(a)_.
  - else if _b \>= 0_ then the result is _abs(a)_
  - else if _b < 0_ it is -_abs(a)_.

### **Examples**

Sample program:
```fortran
program demo_sign
implicit none
  ! basics
   print *,  sign( -12,  1 )
   print *,  sign( -12,  0 )
   print *,  sign( -12, -1 )
   print *,  sign(  12,  1 )
   print *,  sign(  12,  0 )
   print *,  sign(  12, -1 )

   if(sign(1.0,-0.0)== -1.0)then
      print *, 'this processor distinguishes +0 from -0'
   else
      print *, 'this processor does not distinguish +0 from -0'
   endif
   print *,'either way, 0==-0 is true!',0==-0
   print *,'either way, 0=/-0 is false!',0/=-0

   print *,  'elemental', sign( -12.0, [1.0, 0.0, -1.0] )

end program demo_sign
```
Results:
```text
    >        12
    >        12
    >       -12
    >        12
    >        12
    >       -12
    > this processor does not distinguish +0 from -0
    > elemental   12.00000       12.00000      -12.00000
```
### **Standard**

FORTRAN 77

### **See also**

[**abs**(3)](#abs)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
