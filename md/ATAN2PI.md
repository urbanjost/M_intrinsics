## atan2pi

### **Name**

**atan2pi**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Circular Arc tangent (inverse tangent)
function

### **Synopsis**
```fortran
    result = atan2pi(y, x)
```
```fortran
     elemental real(kind=KIND) function atan2pi(y, x)

      real,kind=KIND) :: atan2pi
      real,kind=KIND),intent(in) :: y, x
```
### **Characteristics**

 - **x** and **y** must be reals of the same kind.
 - The return value has the same type and kind as **y** and **x**.

### **Description**

  **atan2pi**(3) computes in half-revolutions a processor-dependent
  approximation of the arctangent of the components of the complex number
  ( **x**, **y** ) or equivalently the principal value of the arctangent
  of the value **y/x** (which determines a unique angle).

  If **y** has the value zero, **x** shall not have the value zero.

  The resulting phase lies in the range -1 <= atan2pi (Y,X) <= 1 and is equal to a
  processor-dependent approximation to a value of arctan(Y/X).

### **Options**

- **y**
  : The imaginary component of the complex value **(x,y)** or the **y**
  component of the point **\<x,y\>**.

- **x**
  : The real component of the complex value **(x,y)** or the **x**
  component of the point **\<x,y\>**.

### **Result**

The value returned is by definition the principal value of the complex
number **(x, y)**, or in other terms, the phase of the phasor x+i\*y.

The principal value is simply what we get when we adjust an angular
half-revolution value to lie between **-1** and **1** inclusive,

The classic definition of the arctangent is the angle that is formed
in Cartesian coordinates of the line from the origin point **\<0,0\>**
to the point **\<x,y\>** .

Pictured as a vector it is easy to see that if **x** and **y** are both
zero the angle is indeterminate because it sits directly over the origin,
so **atan(0.0,0.0)** will produce an error.

Range of returned values by quadrant:
```text
>                   +1/2
>                     |
>                     |
>       1/2 < z < 1   |   0 > z < 1/2
>                     |
>    +-1 -------------+---------------- +-0
>                     |
>       1/2 < -z < 1  |   0 < -z < 1/2
>                     |
>                     |
>                   -1/2
>
     NOTES:

     If the processor distinguishes -0 and +0 then the sign of the
     returned value is that of Y when Y is zero, else when Y is zero
     the returned value is always positive.
```
### **Examples**

Sample program:
```fortran
program demo_atan2pi
real :: z
complex :: c
real, parameter :: h2d = 180.0
 !
 ! basic usage
  ! atan2pi (1.5574077, 1.0) has the value 1.0 (approximately).
  z=atan2pi(1.5574077, 1.0)
  write(*,*) 'half-revolutions=',z,'degrees=',h2d*z
 !
 ! elemental arrays
  write(*,*)'elemental',atan2pi( [10.0, 20.0], [30.0,40.0] )
 !
 ! elemental arrays and scalars
  write(*,*)'elemental',atan2pi( [10.0, 20.0], 50.0 )
 !
 ! break complex values into real and imaginary components
 ! (note TAN2() can take a complex type value )
  c=(0.0,1.0)
  write(*,*)'complex',c,atan2pi( x=c%re, y=c%im )
 !
 ! extended sample converting cartesian coordinates to polar
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
    write(*,'(a)')repeat('1234567890',8)
  do i=1,size(vals)
     ang=atan2pi(vals(i)%im,vals(i)%re)
     write(*,101)vals(i),ang,h2d*ang
  enddo
  101 format(             &
  & 'X= ',f5.2,           &
  & ' Y= ',f5.2,          &
  & ' HALF-REVOLUTIONS= ',f7.3,        &
  & T50,' DEGREES= ',g0.4)
 endblock COMPLEX_VALS
!
end program demo_atan2pi
```
Results:
```text
 >  half-revolutions=  0.318309873     degrees=   57.2957764    
 >  elemental  0.102416381      0.147583619    
 >  elemental   6.28329590E-02  0.121118948    
 >  complex             (0.00000000,1.00000000)  0.500000000    
 > X=  1.00 Y=  0.00 HALF-REVOLUTIONS=   0.000       DEGREES= 0.000
 > X=  1.00 Y=  1.00 HALF-REVOLUTIONS=   0.250       DEGREES= 45.00
 > X=  0.00 Y=  1.00 HALF-REVOLUTIONS=   0.500       DEGREES= 90.00
 > X= -1.00 Y=  1.00 HALF-REVOLUTIONS=   0.750       DEGREES= 135.0
 > X= -1.00 Y=  0.00 HALF-REVOLUTIONS=   1.000       DEGREES= 180.0
 > X= -1.00 Y= -1.00 HALF-REVOLUTIONS=  -0.750       DEGREES= -135.0
 > X=  0.00 Y= -1.00 HALF-REVOLUTIONS=  -0.500       DEGREES= -90.00
```
### **Standard**

Fortran 2023 

### **See Also**

- [**atan**(3)](#atan)

### **Resources**

- [arctan:wikipedia](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)
 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
