## cospi

### **Name**

**cospi**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Circular Cosine function

### **Synopsis**
```fortran
    result = cospi(x)
```
```fortran
     elemental real(kind=KIND) function cospi(x)

      real(kind=KIND),intent(in) :: x
```
### **Characteristics**

 - **x** is of type _real_.
 - **KIND** may be any kind supported by the associated type of **x**.
 - The returned value will be of the same type and kind as the argument
   **x**.

### **Description**

  **cospi**(3) computes the circular cosine of an angle **x** given the
  size of the angle in half-revolutions.

  The cosine of a _real_ value is the ratio of the adjacent side to the
  hypotenuse of a right-angled triangle.

  **cospi(x)** is approximately equal to **cos(x\*PI)**.
    
### **Options**

- **x**
  : The angle in half-revolutions to compute the cosine of.

### **Result**

  The return value is the approximate value of the cosine of **x**.

  The return value lies in the range 
  **-1 \<= cospi(x) \<= 1** .


### **Examples**

  Example: **cospi(1.0)** has the value -1.0 (approximately).

Sample program:
```fortran
program demo_cos
implicit none
character(len=*),parameter :: g2='(a,t21,*(g0,1x))'
   write(*,g2) 'Basics:'
   write(*,g2) 'COSpi(0)=',      cospi(0.0d0)
   write(*,g2) 'COSpi(1)=',      cospi(1.0d0)
   write(*,g2) 'COSpi(1/2)=',    cospi(1.0d0/2.0d0)
   write(*,g2) 'COSpi(2)=',      cospi(2.0d0)
   write(*,g2) 'COSpi(-2)=',     cospi(-2.0d0)
   write(*,g2) 'COSpi(-2000)=',  cospi(-2000.0d0)
   write(*,g2) 'COSpi(3000)=',   cospi(3000.0d0)
   write(*,g2) 'Elemental:'
   write(*,g2) 'COSpi([0,1/4,-1/4])=',COSpi([0.0,0.25,-0.25])
end program demo_cos
```
Results:
```text
 > Basics:
 > COSpi(0)=           1.0000000000000000
 > COSpi(1)=           -1.0000000000000000
 > COSpi(1/2)=         0.61232339957367660E-16
 > COSpi(2)=           1.0000000000000000
 > COSpi(-2)=          1.0000000000000000
 > COSpi(-2000)=       1.0000000000000000
 > COSpi(3000)=        1.0000000000000000
 > Elemental:
 > COSpi([0,1/4,-1/4])=1.00000000 0.707106769 0.707106769
```
### **Standard**

Fortran 2023 

### **See Also**

[**acos**(3)](#acos),
[**sin**(3)](#sin),
[**tan**(3)](#tan)

### **Resources**

- [Wikipedia:sine and cosine](https://en.wikipedia.org/wiki/Sine_and_cosine)

 _fortran-lang intrinsic descriptions_
