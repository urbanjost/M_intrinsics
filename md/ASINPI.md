## asinpi

### **Name**

**asinpi**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Circular arc sine function

### **Synopsis**
```fortran
    result = asinpi(x)
```
```fortran
     elemental real(kind=KIND) function asinpi(x)

      real(kind=KIND),intent(in) :: x
```
### **Characteristics**

 - **KIND** may be any _real_ kind 
 - The returned value will be of the same type and kind as the argument.

### **Description**

**asinpi**(3) computes the arcsine of its argument **x**.

The arcsine is the inverse function of the sine function. It is commonly
used in trigonometry when trying to find the angle when the lengths of
the hypotenuse and the opposite side of a right triangle are known.

The returned value is in half-revolutions (ie. in multiples
of PI).

Example: ASINPI(1:0) has the value 0:5 (approximately).

### **Options**

- **x**
  : The value to compute the arcsine of; where |X| <= 1.
  : The type shall be _real_ 

### **Result**

  The result has a value equal to a processor-dependent approximation
  to the arc sine of X.
  The result is _real_ and it is expressed in half-revolutions
  and lies in the range
```fortran
        -1 <= asinpi (X) <= 1
```
  and is the same kind as the input.

### **Examples**

The arcsine will allow you to find the measure of a right angle when you
know the ratio of the side opposite the angle to the hypotenuse.

So if you knew that a train track rose 1.25 vertical miles on a track
that was 50 miles long, you could determine the average angle of incline
of the track using the arcsine. Given

     sin(theta) = 1.25 miles/50 miles (opposite/hypotenuse)

Sample program:
```fortran
program demo_asinpi
use, intrinsic :: iso_fortran_env, only : dp=>real64
implicit none
! value to convert degrees to half-revolutions
real(kind=dp),parameter :: D2HR=1/180.0_dp
real(kind=dp)           :: angle, rise, run
character(len=*),parameter :: all='(*(g0,1x))'
  ! basics
  ! elemental
  print all, asinpi( [0.0d0, 0.5d0, -0.5d0, 1.0d0, -1.0d0 ])
  !
  ! sample application
  ! given sine(theta) = 1.25 miles/50 miles (opposite/hypotenuse)
  ! then taking the arcsine of both sides of the equality yields
  ! theta = arcsine(1.25 miles/50 miles) ie. arcsine(opposite/hypotenuse)
  rise=1.250_dp
  run=50.00_dp
  angle = asinpi(rise/run)
  print all, 'angle of incline(half-revolutions) = ', angle
  angle = angle/D2HR
  print all, 'angle of incline(degrees) = ', angle
  print all, 'percent grade=',rise/run*100.0_dp
contains
elemental function asinpi(x)
real(kind=dp),parameter  :: PI=acos(-1.0_dp)
real(kind=dp),intent(in) :: x
real(kind=dp)            :: asinpi
   asinpi=asin(x)/PI
end function asinpi
end program demo_asinpi
```
Results:
```text
 > 0.00, 0.166667, -0.166667, 0.50, -0.50
 > angle of incline(half-revolutions) =  0.79585763198139307E-2
 > angle of incline(degrees) =  1.4325437375665075
 > percent grade= 2.5000000000000000
```
The percentage grade is the slope, written as a percent. To calculate
the slope you divide the rise by the run. In the example the rise is
1.25 mile over a run of 50 miles so the slope is 1.25/50 = 0.025.
Written as a percent this is 2.5 %.

For the US, two and 1/2 percent is generally thought of as the upper
limit. This means a rise of 2.5 feet when going 100 feet forward. In
the US this was the maximum grade on the first major US railroad, the
Baltimore and Ohio. Note curves increase the frictional drag on a
train reducing the allowable grade.

### **Standard**

Fortran 2023

### **See Also**

- Inverse function in half-revolutions: [**sinpi**(3)](#sinpi)
- function in radians: [**asin**(3)](#asin)
- function in degrees : [**asind**(3)](#asind)
- radians: [**sin**(3)](#sin)
- degrees: [**sind**(3)](#sind)

### **Resources**

- [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

