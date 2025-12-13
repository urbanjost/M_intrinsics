## exp

### **Name**

**exp**(3) - \[MATHEMATICS\] Base-e exponential function

### **Synopsis**
```fortran
    result = exp(x)
```
```fortran
     elemental TYPE(kind=KIND) function exp(x)

      TYPE(kind=KIND),intent(in) :: x
```
### **Characteristics**

 - **x** may be _real_ or _complex_ of any kind.
 - The return value has the same type and kind as **x**.

### **Description**

**exp**(3) returns the value of _e_ (the base of natural logarithms)
raised to the power of **x**.

"_e_" is also known as _Euler's constant_.

So for either a real or complex scalar X, it returns eˆX , where e is
the base of the natural logarithm (approximately 2.718281828459045).

For real inputs, EXP returns a real result.

If **x** is of type _complex_, its imaginary part is regarded as a value
in radians such that (see _Euler's formula_):
```fortran
    exp((re,im)) = exp(re) * cmplx(cos(im),sin(im),kind=kind(cx))
```
Since **exp**(3) is the inverse function of **log**(3) the maximum valid magnitude
of the _real_ component of **x** is **log(huge(x))**.

**exp** being elemental, when X is an array (real or complex), the
function is applied element‐wise, returning an array of the same shape.

    Numerical Considerations

     For very large real X, the result may overflow to infinity in
     finite‐precision arithmetic. For very small (negative) real X ,
     the result approaches zero. Complex inputs with large imaginary
     parts may produce results with significant numerical errors due
     to the trigonometric functions involved.

### **Options**

- **x**
  : The type shall be _real_ or _complex_.

### **Result**

The value of the result is **e\*\*x** where **e** is Euler's constant.

If **x** is of type complex, its imaginary part is
regarded as a value in radians.

### **Examples**

Sample program:

```fortran
program demo_exp
implicit none
integer,parameter :: dp=kind(0.0d0)
real              :: x, re, im
complex           :: cx
real              :: r_array(3), r_array_result(3)
complex           :: c_array(2), c_array_result(2)
integer           :: i

   x = 1.0
   write(*,*)"Euler's constant is approximately",exp(x)

   !! complex values
   ! given
   re=3.0
   im=4.0
   cx=cmplx(re,im)

   ! complex results from complex arguments are Related to Euler's formula
   write(*,*)'given the complex value ',cx
   write(*,*)'exp(x) is',exp(cx)
   write(*,*)'is the same as',exp(re)*cmplx(cos(im),sin(im),kind=kind(cx))

   ! exp(3) is the inverse function of log(3) so
   ! the real component of the input must be less than or equal to
   write(*,*)'maximum real component',log(huge(0.0))
   ! or for double precision
   write(*,*)'maximum doubleprecision component',log(huge(0.0d0))

   ! but since the imaginary component is passed to the cos(3) and sin(3)
   ! functions the imaginary component can be any real value

   ! Real array example
   r_array = [0.0, 1.0, -1.0]
   r_array_result = exp(r_array)
   do i = 1, size(r_array)
     write(*, '(A, I0, A, F15.10)') "exp(r_array(", i, ")) = ", r_array_result(i)
   enddo

   ! Complex array example
   c_array = [cmplx(0.0, 0.0, kind=dp), cmplx(1.0, 1.0, kind=dp)]
   c_array_result = exp(c_array)
   do i = 1, size(c_array)
     write(*, '(A, I0, A, F15.10, A, F15.10, A)') "exp(c_array(", i, ")) = (", &
     real(c_array_result(i)), ", ", aimag(c_array_result(i)), ")"
   enddo
end program demo_exp
```
Results:
```text
 >  Euler's constant is approximately   2.71828175    
 >  given the complex value              (3.00000000,4.00000000)
 >  exp(x) is           (-13.1287832,-15.2007847)
 >  is the same as           (-13.1287832,-15.2007847)
 >  maximum real component   88.7228394    
 >  maximum doubleprecision component   709.78271289338397     
 > exp(r_array(1)) =    1.0000000000
 > exp(r_array(2)) =    2.7182817459
 > exp(r_array(3)) =    0.3678794503
 > exp(c_array(1)) = (   1.0000000000,    0.0000000000)
 > exp(c_array(2)) = (   1.4686938524,    2.2873551846)
```
### **Standard**

FORTRAN 77

### **See Also**

- [**log**(3)](#log)

### **Resources**

- Wikipedia:[Exponential function](https://en.wikipedia.org/wiki/Exponential_function)

- Wikipedia:[Euler's formula](https://en.wikipedia.org/wiki/Euler%27s_formula)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_

