## cos

### **Name**

**cos**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Cosine function

### **Synopsis**
```fortran
    result = cos(x)
```
```fortran
     elemental TYPE(kind=KIND) function cos(x)

     TYPE(kind=KIND),intent(in) :: x
```
where **TYPE** may be _real_ or _complex_ and **KIND** may be any kind supported
by the associated type. The returned value will be of the same type and kind as
the argument.

### **Description**

**cos(x)** computes the cosine of an angle **x** given the size of the
angle in radians.

The cosine of a _real_ value is the ratio of the adjacent side to the
hypotenuse of a right-angled triangle.

### **Options**

- **x**
  : The angle in radians to compute the cosine of.

### **Result**

The return value is the tangent of **x**.

If **x** is of the type _real_, the return value lies in
the range **-1 \<= cos(x) \<= 1** .

### **Examples**

Sample program:
```fortran
program demo_cos
implicit none
doubleprecision,parameter :: PI=atan(1.0d0)*4.0d0
   write(*,*)'COS(0.0)=',cos(0.0)
   write(*,*)'COS(PI)=',cos(PI)
   write(*,*)'COS(PI/2.0d0)=',cos(PI/2.0d0),' EPSILON=',epsilon(PI)
   write(*,*)'COS(2*PI)=',cos(2*PI)
   write(*,*)'COS(-2*PI)=',cos(-2*PI)
   write(*,*)'COS(-2000*PI)=',cos(-2000*PI)
   write(*,*)'COS(3000*PI)=',cos(3000*PI)
end program demo_cos
```
Results:
```
   COS(0.0)=        1.00000000
   COS(PI)=        -1.0000000000000000
   COS(PI/2.0d0)=   6.1232339957367660E-017
   EPSILON=         2.2204460492503131E-016
   COS(2*PI)=       1.0000000000000000
   COS(-2*PI)=      1.0000000000000000
   COS(-2000*PI)=   1.0000000000000000
```
### **Standard**

FORTRAN 77 and later

### **See Also**

[**acos**(3)](#acos),
[**sin**(3)](#sin),
[**tan**(3)](#tan)

### **Resources**

- [Wikipedia:sine and cosine](https://en.wikipedia.org/wiki/Sine_and_cosine)

 _fortran-lang intrinsic descriptions_
