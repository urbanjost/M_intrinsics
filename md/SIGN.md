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

where TYPE may be _real_ or _integer_ and KIND is any supported kind
for the type.

### **Description**

**sign**(3) returns a value with the magnitude of _a_ but with the
sign of _b_.

For processors that distinguish between positive and negative zeros
_sign()_ may be used to distinguish between _real_ values 0.0 and
-0.0. SIGN (1.0, -0.0) will return  -1.0 when a negative zero is
distinguishable.

### **Options**

  - **a**
    : The value whos magnitude will be returned. Shall be of type
    _integer_ or _real_

  - **b**
    : The value whose sign will be returned. Shall be of the same type
    and kind as **a**

### **Result**

The kind of the return value is the magnitude of _a_ with the sign of
_b_. That is,

  - If _b \>= 0_ then the result is _abs(a)_
  - else if _b < 0_ it is -_abs(a)_.
  - if _b_ is _real_ and the processor distinguishes between _-0.0_
    and _0.0_ then the
    result is _-abs(a)_

### **Examples**

Sample program:
```fortran
program demo_sign
implicit none
   print *,  sign( -12,  1 )
   print *,  sign( -12,  0 )
   print *,  sign( -12, -1 )

   print *,  sign( -12.0, [1.0, 0.0, -1.0] )

   print *,  'can I distinguish 0 from -0? ', &
   &  sign( 1.0, -0.0 ) .ne. sign( 1.0, 0.0 )
end program demo_sign
````
Results:

```text
             12
             12
            -12
      12.00000       12.00000      -12.00000
    can I distinguish 0 from -0?  F
```
### **Standard**

### **See also**

[****(3)](#)


FORTRAN 77

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
