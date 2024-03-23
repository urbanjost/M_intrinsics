## gamma

### **Name**

**gamma**(3) - \[MATHEMATICS\] Gamma function, which yields factorials for positive whole numbers

### **Synopsis**
```fortran
    result = gamma(x)
```
```fortran
     elemental real(kind=**) function gamma( x)

      type(real,kind=**),intent(in) :: x
```
### **Characteristics**

 - **x** is a _real_ value of any available KIND
 - returns a _real_ value with the same kind as **x**.

### **Description**

  **gamma(x)** computes Gamma of **x**. For positive whole number values of **n** the
  Gamma function can be used to calculate factorials, as **(n-1)! == gamma(real(n))**.
  That is
```text
n! == gamma(real(n+1))
```
$$
\\__Gamma__(x) = \\int\_0\*\*\\infty
t\*\*{x-1}{\\mathrm{e}}\*\*{__-t__}\\,{\\mathrm{d}}t
$$

### **Options**

- **x**
  : Shall be of type _real_ and neither zero nor a negative integer.

### **Result**

  The return value is of type _real_ of the same kind as _x_. The result
  has a value equal to a processor-dependent approximation to the gamma
  function of **x**.

### **Examples**

Sample program:

```fortran
program demo_gamma
use, intrinsic :: iso_fortran_env, only : wp=>real64, int64
implicit none
real :: x, xa(4)
integer :: i, j

   ! basic usage
   x = gamma(1.0)
   write(*,*)'gamma(1.0)=',x

   ! elemental
   xa=gamma([1.0,2.0,3.0,4.0])
   write(*,*)xa
   write(*,*)


   ! gamma() is related to the factorial function
   do i = 1, 171
      ! check value is not too big for default integer type
      if (factorial(i)  <=  huge(0)) then
         write(*,*) i, nint(factorial(i)), 'integer'
      elseif (factorial(i)  <=  huge(0_int64)) then
         write(*,*) i, nint(factorial(i),kind=int64),'integer(kind=int64)'
      else
         write(*,*) i, factorial(i) , 'user factorial function'
         write(*,*) i, product([(real(j, kind=wp), j=1, i)]), 'product'
         write(*,*) i, gamma(real(i + 1, kind=wp)), 'gamma directly'
      endif
   enddo


contains
function factorial(i) result(f)
!  GAMMA(X) computes Gamma of X. For positive whole number values of N the
!  Gamma function can be used to calculate factorials, as (N-1)! ==
!  GAMMA(REAL(N)). That is
!
!      n! == gamma(real(n+1))
!
integer, intent(in) :: i
real(kind=wp) :: f
   if (i  <=  0) then
      write(*,'(*(g0))') '<ERROR> gamma(3) function value ', i, ' <= 0'
      stop '<STOP> bad value in gamma function'
   endif
   f = anint(gamma(real(i + 1,kind=wp)))
end function factorial

end program demo_gamma
```
Results:
```text
 >  gamma(1.0)=   1.00000000
 >    1.00000000       1.00000000       2.00000000       6.00000000
 >
 >            1           1 integer
 >            2           2 integer
 >            3           6 integer
 >            4          24 integer
 >            5         120 integer
 >            6         720 integer
 >            7        5040 integer
 >            8       40320 integer
 >            9      362880 integer
 >           10     3628800 integer
 >           11    39916800 integer
 >           12   479001600 integer
 >           13           6227020800 integer(kind=int64)
 >           14          87178291200 integer(kind=int64)
 >           15        1307674368000 integer(kind=int64)
 >           16       20922789888000 integer(kind=int64)
 >           17      355687428096000 integer(kind=int64)
 >           18     6402373705728001 integer(kind=int64)
 >           19   121645100408832000 integer(kind=int64)
 >           20  2432902008176640000 integer(kind=int64)
 >           21   5.1090942171709440E+019 user factorial function
 >           21   5.1090942171709440E+019 product
 >           21   5.1090942171709440E+019 gamma directly
 >            :
 >            :
 >            :
 >          170   7.2574156153079990E+306 user factorial function
 >          170   7.2574156153079940E+306 product
 >          170   7.2574156153079990E+306 gamma directly
 >          171                  Infinity user factorial function
 >          171                  Infinity product
 >          171                  Infinity gamma directly
```

### **Standard**

Fortran 2008

### **See Also**

Logarithm of the Gamma function: [**log_gamma**(3)](#log_gamma)

### **Resources**

[Wikipedia: Gamma_function](https://en.wikipedia.org/wiki/Gamma_function)

 _fortran-lang intrinsic descriptions_
