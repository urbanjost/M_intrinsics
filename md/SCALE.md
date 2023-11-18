## scale

### **Name**

**scale**(3) - \[MODEL_COMPONENTS\] Scale a real value by a whole power of the radix

### **Synopsis**
```fortran
    result = scale(x, i)
```
```fortran
     elemental real(kind=KIND) function scale(x, i)

      real(kind=KIND),intent(in)   :: x
      integer(kind=**),intent(in)  :: i
```
### **Characteristics**

   - **x** is type _real_ of any kind
   - **i** is type an _integer_ of any kind
   - the result is _real_ of the same kind as **x**

### **Description**

   **scale**(3) returns x \* **radix(x)\*\*i**.

   It is almost certain the radix(base) of the platform is two, therefore
   **scale**(3) is generally the same as **x*2\*\*i**

### **Options**

- **x**
  : the value to multiply by **radix(x)\*\*i**. Its type and kind is used
  to determine the radix for values with its characteristics and determines
  the characteristics of the result, so care must be taken the returned
  value is within the range of the characteristics of **x**.

- **i**
  : The power to raise the radix of the machine to

### **Result**

The return value is **x \* radix(x)\*\*i**, assuming that value can be
represented by a value of the type and kind of **x**.

### **Examples**

Sample program:
```fortran
program demo_scale
implicit none
real :: x 
complex :: c
integer :: i
   x = 1.0
   print *, (scale(x,i),i=1,5)
   x = 3.0
   print *, (scale(x,i),i=1,5)
   print *, (scale(log(1.0),i),i=1,5)
   ! on modern machines radix(x) is almost certainly 2
   x = 178.1387e-4
   i = 5
   print *, x, i, scale(x, i), x*radix(x)**i
   ! x*radix(x)**i is the same except roundoff errors are not restricted
   i = 2
   print *, x, i, scale(x, i), x*radix(x)**i
   ! relatively easy to do complex values as well
   c=(3.0,4.0)
   print *, c, i, scale_complex(c, i)!, c*radix(c)**i
contains
   function scale_complex(x, n)
   ! example supporting complex value for default kinds
      complex, intent(in) :: x
      integer, intent(in) :: n
      complex :: scale_complex
      scale_complex = cmplx(scale(x%re, n), scale(x%im, n), kind=kind(x%im))
   end function scale_complex
end program demo_scale
```
Results:
```text
 > 2.00000000 4.00000000  8.00000000     16.0000000 32.0000000    
 > 6.00000000 12.0000000 24.0000000      48.0000000 96.0000000    
 > 0.00000000 0.00000000  0.00000000     0.00000000 0.00000000    
 > 1.78138707E-02    5   0.570043862      0.570043862    
 > 1.78138707E-02    2   7.12554827E-02   7.12554827E-02
 > (3.00000000,4.00000000) 2 (12.0000000,16.0000000)

### **Standard**

Fortran 95

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
[**rrspacing**(3)](#rrspacing),
[**set_exponent**(3)](#set_exponent),
[**spacing**(3)](#spacing),
[**tiny**(3)](#tiny)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
