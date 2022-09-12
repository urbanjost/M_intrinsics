## atan2

### **Name**

**atan2**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arctangent (inverse tangent)
function

### **Syntax**

```fortran
  elemental function atan2(y, x)

    type(real,kind=KIND) :: atan2
    type(real,kind=KIND),intent(in) :: y, x
```
The return value has the same type and kind type parameter as **y**.
### **Description**

**atan2(y, x)** computes the arctangent of the complex number
( **x** + i **y** ).

This function can be used to transform from Cartesian into polar
coordinates and allows to determine the angle in the correct quadrant.

To convert from Cartesian Coordinates **(x,y)** to polar coordinates

(r,theta): $$ \begin{aligned} r &= \sqrt{x**2 + y**2} \\ \theta
&= \tan\*\*{**-1**}(y / x) \end{aligned} $$

### **Arguments**

- **y**
  : The type shall be _real_.

- **x**
  : The type and kind type parameter shall be the same as **y**. If **y** is
  zero, then **x** must be nonzero.

**result**

The resulting value is a processor-dependent approximation of the
arctangent of **y/x** in radians.

The characteristics of the result are the same as the elements of **x**
and **y**.

The returned value will be in the range **-PI \<= atan(x) \<= PI**. That
is, the absolute value returned is <= **PI**. It is _not_ from **0**
to **2*PI**.

The classic definition of the arctangent is the angle that is formed
in Cartesian coordinates of the line from the origin point **\<0,0\>**
to the point **\<x,y\>** .

It is  also by definition the principal value of the complex number
**(x+i, y)** where **i=sqrt(-1.0)**.

Note that if pictured as the angle formed by the line from <0,0> to
<x,y> that if **x** and **y** are both zero the angle is indeterminent
because it sits directly over the origin, so **atan(0.0,0.0)** results
in an error.

Range of returned values by quadrant:
```text
                   +PI/2
                     |
                     |
        PI/2<Z<PI    |   0>Z<PI/2
                     |
   +-PI -------------+---------------- +-0
                     |
         PI/2<-Z<PI  |    0<-Z<PI/2
                     |
                     |
     	           -PI/2
		-
     NOTES:

     If the processor distinguishes -0 and +0 then the sign of the
     returned value is that of Y when Y is zero, else when Y is zero
     the returned value is always positive.
```

### **Examples**

Sample program:
```fortran
program demo_atan2
real :: x, y, z

 ! basic usage
  ! ATAN2 (1.5574077, 1.0) has the value 1.0 (approximately).
  z=atan2(1.5574077, 1.0)
  write(*,*) 'radians=',z,'degrees=',r2d(z)

 ! elemental arrays
  write(*,*)'elemental',atan2( [10.0, 20.0], [30.0,40.0] )
 ! elemental arrays and scalars
  write(*,*)'elemental',atan2( [10.0, 20.0], 50.0 )

 ! use with complex values
  COMPLEX_VALS: block
  real                :: ang, radius
  complex,allocatable :: vals(:)

  vals=[ &
    ( 0.0, 1.0 ), & 
    ( 1.0, 1.0 ), &
    ( 1.0, 0.0 ), &
    ( 0.0,-1.0 ), &
    (-1.0, 1.0 ), &
    (-1.0, 0.0 ), &
    (-1.0,-1.0 )]
  do i=1,size(vals)
     call cartesian_to_polar(vals(i)%im,vals(i)%re,radius,ang)
     write(*,101)vals(i),ang,r2d(ang),radius
  enddo
  101 format('Y= ',f5.2,' X= ',f5.2,' ANGLE= ',g0,T40,'DEGREES= ',g0.4,T57,'DISTANCE=',g0)
 endblock COMPLEX_VALS

contains

elemental real function r2d(radians)
! input radians to convert to degrees
doubleprecision,parameter :: DEGREE=0.017453292519943d0 ! radians
real,intent(in)           :: radians     
   r2d=radians / DEGREE ! do the conversion
end function r2d

subroutine cartesian_to_polar(x,y,radius,inclination)
implicit none
real,intent(in)  :: x,y
real,intent(out) :: radius,inclination
   radius=sqrt(x**2+y**2)
   if(radius.eq.0)then
      inclination=0.0
   else
      inclination=atan2(y,x)
   endif
end subroutine cartesian_to_polar

end program demo_atan2
```
### **See Also**

[**atan**(3)](ATAN)

### **Standard**

FORTRAN 77 and later

_fortran-lang intrinsic descriptions_




