## asind

### **Name**

**asind**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arcsine function in degrees

### **Synopsis**
```fortran
    result = asind(x)
```
```fortran
     elemental real(kind=KIND) function asind(x)

      real(kind=KIND) :: x
```
### **Characteristics**

 - **KIND** may be any kind supported by the _real_ type.
 - The returned value will be of the same type and kind as the argument.

### **Description**

**asind**(3) computes the arc sine of its argument **x** in degrees

The arcsine is the inverse function of the sine function. It is commonly
used in trigonometry when trying to find the angle when the lengths of
the hypotenuse and the opposite side of a right triangle are known.

Example: **asind(1.0)** has the value 90.0 (approximately).

### **Options**

- **x**
  : The value to compute the arc sine of
  : The type shall be _real_ and a magnitude that is less than or
  : equal to one |X| <= 1.
  : It is expressed in degrees and lies in the range 90 <= asind(x) <= 90.

### **Result**

  The result has a value equal to a processor-dependent approximation
  to arcsin(x).

  If **x** is real the result is _real_ and it is expressed in radians
  and lies in the range
```fortran
        PI/2 <= asind (X) <= PI/2.
```
  If the argument (and therefore the result) is imaginary the real part
  of the result is in radians and lies in the range
```fortran
    -PI/2 <= real(asind(x)) <= PI/2
```
### **Examples**

The arcsine will allow you to find the measure of a right angle when you
know the ratio of the side opposite the angle to the hypotenuse.

So if you knew that a train track rose 1.25 vertical miles on a track
that was 50 miles long, you could determine the average angle of incline
of the track using the arcsine. Given

     sin(theta) = 1.25 miles/50 miles (opposite/hypotenuse)

Sample program:
```fortran
program demo_asind
use, intrinsic :: iso_fortran_env, only : dp=>real64
implicit none
! value to convert degrees to radians
real(kind=dp),parameter :: R2D=180.0_dp/acos(-1.0_dp)
real(kind=dp)           :: angle, rise, run
character(len=*),parameter :: all='(*(g0,1x))'
  ! given sine(theta) = 1.25 miles/50 miles (opposite/hypotenuse)
  ! then taking the arcsine of both sides of the equality yields
  ! theta = arcsine(1.25 miles/50 miles) ie. arcsine(opposite/hypotenuse)
  rise=1.250_dp
  run=50.00_dp
  angle = asind(rise/run)
  print all, 'angle of incline(degrees) = ', angle
  angle = angle/R2D
  print all, 'angle of incline(radians) = ', angle

  print all, 'percent grade=',rise/run*100.0_dp
contains
subroutine sub1()
! notice the (incidently empty) type is defined below 
! the implicit statement
implicit type(nil) (a)
type nil
end type nil
type(nil) :: anull
end subroutine sub1
end program demo_asind
```
Results:
```text
 > angle of incline(degrees) =  1.4325437375665075
 > angle of incline(radians) =  0.25002604899361135E-1
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

Inverse function: [**sin**(3)](#sin)

### **Resources**

- [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
