## cosd

### **Name**

**cosd**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Degree cosine function

### **Synopsis**
```fortran
    result = cosd(x)
```
```fortran
     elemental real(kind=KIND) function cosd(x)

      real(kind=KIND),intent(in) :: x
```
### **Characteristics**

 - **x** is of type _real_ of any valid kind.
 - **KIND** may be any real kind.
 - The returned value will be of the same type and kind as the argument
   **x**.

### **Description**

  **cosd**(3) computes the cosine of an angle **x** given the size of
  the angle in degrees.

  The cosine is the ratio of the adjacent side to the hypotenuse of a
  right-angled triangle.

### **Options**

- **x**
  : The angle in degrees to compute the cosine of.

### **Result**

  The return value is an approximation of the cosine of **x**.

  The return value lies in the range
```code
  -1 \<= cosd(x) \<= 1
```
### **Examples**

cosd(180.0) has the value -1.0 (approximately).

Sample program:
```fortran
program demo_cosd
implicit none
character(len=*),parameter :: g2='(a,t20,g0)'
   write(*,g2)'cosd(0.0)=',cosd(0.0)
   write(*,g2)'cosd(180.0)=',cosd(180.0)
   write(*,g2)'cosd(90.0d0)=',cosd(90.0d0)
   write(*,g2)'cosd(360.0)=',cosd(360.0)
   write(*,g2)'cosd(-360.0)=',cosd(-360.0)
   write(*,g2)'cosd(-2000*180.0)=',cosd(-2000*180.0)
   write(*,g2)'cosd(3000*180.0)=',cosd(3000*180.0)
end program demo_cosd
```
Results:
```text
 > cosd(0.0)=         1.00000000
 > cosd(180.0)=       -1.00000000
 > cosd(90.0d0)=      0.0000000000000000
 > cosd(360.0)=       1.00000000
 > cosd(-360.0)=      1.00000000
 > cosd(-2000*180.0)= 1.00000000
 > cosd(3000*180.0)=  1.00000000
```
### **Standard**

Fortran 2023

### **See Also**

[**acosd**(3)](#acosd),
[**acos**(3)](#acos),
[**sind**(3)](#sind),
[**tand**(3)](#tand)

### **Resources**

- [Wikipedia:sine and cosine](https://en.wikipedia.org/wiki/Sine_and_cosine)

 _Fortran intrinsic descriptions_
