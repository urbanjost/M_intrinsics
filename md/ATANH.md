## atanh

### **Name**

**atanh**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse hyperbolic tangent function

### **Synopsis**
```fortran
    result = atanh(x)
```
```fortran
     elemental TYPE(kind=KIND) function atanh(x)

      TYPE(kind=KIND),intent(in) :: x
```
### **Characteristics**

 - **TYPE** may be _real_ or _complex_ 
 - **KIND** may be any kind supported by the associated type. 
 - The returned value will be of the same type and kind as the argument.

### **Description**

**atanh(x)** computes the inverse hyperbolic tangent of **x**.

### **Options**

- **x**
  : The type shall be _real_ or _complex_.

### **Result**

The return value has same type and kind as **x**. If **x** is _complex_, the
imaginary part of the result is in radians and lies between
```fortran
     **-PI/2 <= aimag(atanh(x)) <= PI/2**
```
### **Examples**

Sample program:
```fortran
program demo_atanh
implicit none
real, dimension(3) :: x = [ -1.0, 0.0, 1.0 ]

   write (*,*) atanh(x)

end program demo_atanh
```
Results:
```text
   -Infinity   0.00000000             Infinity
```
### **Standard**

Fortran 2008

### **See Also**

Inverse function: [**tanh**(3)](#tanh)

### **Resources**

- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

 _fortran-lang intrinsic descriptions_
