## exponent

### **Name**

**exponent**(3) - \[MODEL:COMPONENTS\] Exponent of floating-point number

### **Synopsis**
```fortran
    result = exponent(x)
```
```fortran
    elemental integer function exponent(x)

     real(kind=**),intent(in) :: x
```
### **Characteristics**
 - **x** shall be of type _real_ of any valid kind
 - the result is a default _integer_ type

### **Description**

  **exponent**(3) returns the value of the exponent part of **x**, provided
  the exponent is within the range of default _integers_.

### **Options**

- **x**
  : the value to query the exponent of

### **Result**

  **exponent**(3) returns the value of the exponent part of **x**

  If **x** is zero the value returned is zero.

  If **x** is an IEEE infinity or NaN, the result has the value HUGE(0).

### **Examples**

Sample program:
```fortran
program demo_exponent
implicit none
real    :: x
integer :: i
   print *, 'basic usage'
   print *, exponent([2.0,32.0,256.0,0.25])
   print *, exponent([1.0,10.0,100.0])
   print '(g0,1x,a,g0,1x,b32.32)', 500.0, 'exponent(500.0)=', &
   exponent(500.0), 500.0
   print '(g0,1x,a,g0,1x,b32.32)', 512.0, 'exponent(512.0)=', &
   exponent(512.0), 512.0
   print '(g0,1x,a,g0,1x,b32.32)', 550.0, 'exponent(550.0)=', &
   exponent(550.0), 525.0
   print *,'==>',log([500.0,512.0,550.0])/log(2.0)
   x=9.31
   i = exponent(x)
   print *, i ,  x

   print *, 'elemental'
   print *, exponent([10.0,100.0,1000.0,-10000.0])

   ! beware of overflow, it may occur silently
   !print *, 2**[10.0,100.0,1000.0,-10000.0]

   print *, 'exponent range'
   print *, minexponent(0.0),    maxexponent(0.0)
   print *, exponent(tiny(0.0)), exponent(huge(0.0))
   call dusty_corners()
contains
subroutine dusty_corners()
use, intrinsic :: ieee_arithmetic
real :: my_inf, my_neg_inf
real :: my_qnan, my_snan

   print *
   print *, 'exponent(0.0)=', exponent(0.0)
   print *
   ! Generate positive infinity
   my_inf = ieee_value(my_inf, ieee_positive_inf)
   !print "(A,b32.32)" ,'in binary format      = ',my_inf
   print *, 'ieee_value(my_inf, ieee_positive_inf) =', my_inf
   print *
   ! Generate negative infinity
   my_neg_inf = ieee_value(my_neg_inf, ieee_negative_inf)
   print *,'ieee_value(my_inf, ieee_neg_inf)', my_neg_inf
   print *
   print *,'exponent([my_inf,my_neg_inf]) =',exponent([my_inf,my_neg_inf])

   if (ieee_support_nan(x)) then
   
      print *
      my_qnan = ieee_value(my_qnan, ieee_quiet_nan)
      print *, 'ieee_value(my_qnan, ieee_quiet_nan) =', my_qnan
      my_snan = ieee_value(my_snan, ieee_signaling_nan)
      print *, 'ieee_value(my_snan, ieee_signaling_nan) =', my_snan
      print *, 'exponent([my_qnan,my_snan]) =',exponent([my_qnan,my_snan])
      print *
      print *, 'Not sure ...'
      print *, 'exponent(tiny(0.0)/2)=', exponent(tiny(0.0)/2)
   
   endif
end subroutine dusty_corners

end program demo_exponent
```
Results:
```text
  >  basic usage
  >            4   9.31000042    
  >  elemental
  >            4           7          10          14
  >  exponent range
  >         -125         128
  >         -125         128
  > 
  >  exponent(0.0)=           0
  > 
  >  ieee_value(my_inf, ieee_positive_inf) =         Infinity
  > 
  >  ieee_value(my_inf, ieee_neg_inf)        -Infinity
  > 
  >  exponent([my_inf,my_neg_inf]) =  2147483647  2147483647
  > 
  >  ieee_value(my_qnan, ieee_quiet_nan) =              NaN
  >  ieee_value(my_snan, ieee_signaling_nan) =              NaN
  >  exponent([my_qnan,my_snan]) =  2147483647  2147483647
  > 
  >  Not sure ...
  >  exponent(tiny(0.0)/2)=        -126
```
### **Standard**

Fortran 95

### **See Also**

[**digits**(3)](#digits),
[**epsilon**(3)](#epsilon),
[**fraction**(3)](#fraction),
[**huge**(3)](#huge),
[**maxexponent**(3)](#maxexponent),
[**minexponent**(3)](#minexponent),
[**nearest**(3)](#nearest),
[**precision**(3)](#precision),
[**radix**(3)](#radix),
[**range**(3)](#range),
[**rrspacing**(3)](#rrspacing),
[**scale**(3)](#scale),
[**set_exponent**(3)](#set_exponent),
[**spacing**(3)](#spacing),
[**tiny**(3)](#tiny)

 _Fortran intrinsic descriptions_
