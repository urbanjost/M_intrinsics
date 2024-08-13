## atan2d

### **Name**

**atan2d**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arc tangent function in
degrees (inverse tangent)

### **Synopsis**
```fortran
    result = atan2d(y, x)
```
```fortran
     elemental real(kind=KIND) function atan2d(y, x)

      real,kind=KIND) :: atan2d
      real,kind=KIND),intent(in) :: y, x
```
### **Characteristics**

 - **x** and **y** must be reals of the same kind.
 - The return value has the same type and kind as **y** and **x**.

### **Description**

  **atan2d**(3) computes in degrees a processor-dependent approximation
  of the arctangent of the principal value of the arctangent of the
  value **y/x** (which determines a unique angle).

  If **y** has the value zero, **x** shall not have the value zero.

  The resulting phase lies in the range -180 <= atan2d (Y,X) <= 180 and is
  equal to a processor-dependent approximation to a value of arctan(Y/X)
  expressed in degrees.

  It is equivalent to **ATAN2(Y, X)\*180/PI** but limited to real values.

### **Options**

- **y**
  : The imaginary component of the complex value **(x,y)** or the **y**
  component of the point **\<x,y\>**.

- **x**
  : The real component of the complex value **(x,y)** or the **x**
  component of the point **\<x,y\>**.

### **Result**
The result is in degrees, not radians.

The radian value is by definition the principal value of the complex
number **(x, y)**, or in other terms, the phase of the phasor x+i\*y.

The principal value is simply what we get when we adjust the value
to lie between **-180** and **180** degrees inclusive,

The classic definition of the arctangent is the angle that is formed
in Cartesian coordinates of the line from the origin point **\<0,0\>**
to the point **\<x,y\>** .

Pictured as a vector it is easy to see that if **x** and **y** are both
zero the angle is indeterminate because it sits directly over the origin,
so **atan2d(0.0,0.0)** will produce an error.

Range of returned values by quadrant:
```text
>                   +90
>                     |
>                     |
>     90 < z < 180    |   0 > z < 90
>                     |
>   +-180 ------------+---------------- +-0
>                     |
>     90 < -z < 180   |   0 < -z < 90
>                     |
>                     |
>                   -90
>
     NOTES:

     If the processor distinguishes -0 and +0 then the sign of the
     returned value is that of Y when Y is zero, else when Y is zero
     the returned value is always positive.
```
### **Examples**

Sample program:
```fortran
program demo_atan2d
implicit none
integer,parameter  :: wp=kind(0.0)
real(wp),parameter :: d2r=acos(-1.0_wp)/180.0_wp
real :: z
complex :: c
 !
 ! basic usage
  ! atan2d (1.5574077, 1.0) has the value 1.0 radian (approximately).
  z=atan2d(1.5574077, 1.0)
  write(*,*) 'degrees=',z,'radians=',d2r*z
 !
 ! elemental arrays
  write(*,*)'elemental',atan2d( [10.0, 20.0], [30.0,40.0] )
 !
 ! elemental arrays and scalars
  write(*,*)'elemental',atan2d( [10.0, 20.0], 50.0 )
 !
 ! multi-dimensional returns multi-dimensional
 write(*,*) atan2(reshape([1.0,1.0,1.0,1.0],[2,2]),&
 & reshape([1.0,1.0,1.0,1.0],[2,2]) )
 !
 ! break complex values into real and imaginary components
  c=(0.0,1.0)
  write(*,*)'complex value treated as components', &
  & c,atan2d( x=c%re, y=c%im )
 !
 ! extended sample
  COMPLEX_VALS: block
  real                :: ang
  complex,allocatable :: vals(:)
  integer             :: i
 !
  vals=[ &
    ( 1.0, 0.0 ), & ! 0
    ( 1.0, 1.0 ), & ! 45
    ( 0.0, 1.0 ), & ! 90
    (-1.0, 1.0 ), & ! 135
    (-1.0, 0.0 ), & ! 180
    (-1.0,-1.0 ), & ! 225
    ( 0.0,-1.0 )]   ! 270
  do i=1,size(vals)
     ang=atan2d(vals(i)%im, vals(i)%re)
     write(*,101)vals(i),ang,d2r*ang
  enddo
  101 format(             &
  & 'X= ',f5.2,           &
  & ' Y= ',f5.2,          &
  & ' ANGLE= ',g0,        &
  & T38,'RADIANS= ',g0.4)
 endblock COMPLEX_VALS
!
end program demo_atan2d
```
Results:
```text
 >  degrees=   57.2957802     radians=   1.00000000    
 >  elemental   18.4349480       26.5650520    
 >  elemental   11.3099327       21.8014107    
 >   0.785398185  0.785398185 0.785398185  0.785398185    
 >  complex value treated as components (0.0000,1.0000) 90.000
 > X=  1.00 Y=  0.00 ANGLE= 0.00000000  RADIANS= 0.000
 > X=  1.00 Y=  1.00 ANGLE= 45.0000000  RADIANS= 0.7854
 > X=  0.00 Y=  1.00 ANGLE= 90.0000000  RADIANS= 1.571
 > X= -1.00 Y=  1.00 ANGLE= 135.000000  RADIANS= 2.356
 > X= -1.00 Y=  0.00 ANGLE= 180.000000  RADIANS= 3.142
 > X= -1.00 Y= -1.00 ANGLE= -135.000000 RADIANS= -2.356
 > X=  0.00 Y= -1.00 ANGLE= -90.0000000 RADIANS= -1.571
```
### **Standard**

Fortran 2023

### **See Also**

- [**atan**(3)](#atan)
- [**atanpi**(3)](#atanpi)

### **Resources**

- [arctan:wikipedia](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
