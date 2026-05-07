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

**asind**(3) computes the arcsine of its argument **x** in degrees.

The arcsine will allow you to find the measure of a right angle when you
know the ratio of the side opposite the angle to the hypotenuse.

The arcsine is the inverse function of the sine function. It is commonly
used in trigonometry when trying to find the angle when the lengths of
the hypotenuse and the opposite side of a right triangle are known.

Example: **asind(1.0)** has the value 90.0 (approximately).

### **Options**

- **x**
  : The value to compute the arc sine of.
    The type shall be _real_ and a magnitude that is less than or
    equal to one |X| <= 1.
    It is expressed in degrees and lies in the range -90 <= asind(x) <= 90.

### **Result**

  The result has a value equal to a processor-dependent approximation
  to arcsin(x).

  The result is expressed in degrees and lies in the range
```fortran
        -90 <= asind (X) <= 90.
```
### **Examples**

So if you knew that a train track rose 1.25 vertical miles on a track
that was 50 miles long, what is the angle of incline of the track?

The percentage grade is the slope, written as a percent. To calculate
the slope you divide the rise by the run.  so the slope is 1.25/50 =
0.025. Written as a percent this is 2.5 %.

For the US, 2 1/2 percent, or a rise of 2.5 feet when going 100 feet
forward, is generally thought of as the upper limit of the grade on
straight track. This was the maximum grade on the first major US railroad,
the Baltimore and Ohio. Note curves in the track increase the frictional
drag on a train reducing the allowable grade.

Sample program:
```fortran
program demo_asind
use, intrinsic :: iso_fortran_env, only : dp=>real64
implicit none
! value to convert degrees to radians
real(kind=dp),parameter    :: R2D=180.0_dp/acos(-1.0_dp)
real(kind=dp)              :: angle, grade, rise, run
character(len=*),parameter :: all='(*(g0,1x))'
integer                    :: i
  ! given sine(theta) = 1.25 miles/50 miles (opposite/hypotenuse)
  ! then taking the arcsine of both sides of the equality yields
  ! theta = arcsine(1.25 miles/50 miles) ie. arcsine(opposite/hypotenuse)
  rise=1.250_dp
  run=50.00_dp
  angle = asind(rise/run)
  print all, 'angle of incline(degrees) = ', angle
  angle = angle/R2D
  print all, 'angle of incline(radians) = ', angle
  print all, 'angle of incline(radians) = ', asin(rise/run)

  print all, 'percent grade=',rise/run*100.0_dp
  print all,
  do i=-360,360,45
     grade=i/360.0d0
     print *, grade, asind(grade), sind(asind(grade))
  enddo
end program demo_asind
```
Results:
```text
    > angle of incline(degrees) =  1.4325437375665075
    > angle of incline(radians) =  0.25002604899361135E-1
    > angle of incline(radians) =  0.25002604899361139E-1
    > percent grade= 2.5000000000000000
    >
    >   -1.0000000000000000  -90.000000000000000   -1.0000000000000000
    >  -0.87500000000000000  -61.044975628140158  -0.87500000000000000
    >  -0.75000000000000000  -48.590377890729144  -0.75000000000000000
    >  -0.62500000000000000  -38.682187453489441  -0.62500000000000000
    >  -0.50000000000000000  -30.000000000000004  -0.50000000000000011
    >  -0.37500000000000000  -22.024312837042164  -0.37500000000000006
    >  -0.25000000000000000  -14.477512185929925  -0.25000000000000006
    >  -0.12500000000000000  -7.1807557814582816  -0.12500000000000000
    >    0.0000000000000000   0.0000000000000000   0.0000000000000000
    >   0.12500000000000000   7.1807557814582816   0.12500000000000000
    >   0.25000000000000000   14.477512185929925   0.25000000000000006
    >   0.37500000000000000   22.024312837042164   0.37500000000000006
    >   0.50000000000000000   30.000000000000004   0.50000000000000011
    >   0.62500000000000000   38.682187453489441   0.62500000000000000
    >   0.75000000000000000   48.590377890729144   0.75000000000000000
    >   0.87500000000000000   61.044975628140158   0.87500000000000000
    >    1.0000000000000000   90.000000000000000    1.0000000000000000
```
### **Standard**

Fortran 2023

### **See Also**

Inverse function: [**sin**(3)](#sin)

### **Resources**

- [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
