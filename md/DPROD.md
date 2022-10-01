## dprod

### **Name**

**dprod**(3) - \[NUMERIC\] Double precision real product

### **Synopsis**
```fortran
    result = dprod(x,y)
```
```fortran
     elemental function dprod(x,y)

     real,intent(in) :: x
     real,intent(in) :: y
     doubleprecision :: dprod
```
### **Characteristics**

**x** and **y** must both be real values of default kind.

The return value is doubleprecision (ie. _real(kind=kind(0.0d0))_).

The setting of compiler options specifying the size of a default _real_
can affect this function.

### **Description**

**dprod(x,y)** produces a _doubleprecision_ product of default _real_
values **x** and **y**.

That is, it is expected to convert the arguments to double precision
before multiplying, which a simple expression **x\*y** would not be
required to do. This can be significant in specialized computations
requiring high precision.

The result has a value equal to a processor-dependent approximation to
the product of **x** and **y**. Note it is recommended in the standard
that the processor compute the product in double precision, rather than
in single precision then converted to double precision; but is only
a recommendation.

### **Options**

- **x**
  : the multiplier

- **y**
  : the multiplicand

### **Result**

The returned value of the product should have the same value as
**dble(x)\*dble(y)**.

### **Examples**

Sample program:

```fortran
program demo_dprod
implicit none
integer,parameter :: dp=kind(0.0d0)
real :: x = 5.2
real :: y = 2.3
doubleprecision :: xx
real(kind=dp)   :: dd

   print *,'algebraically 5.2 x 2.3 is exactly 11.96'
   print *,'as floating point values results may differ slightly:'
   ! basic usage
   dd = dprod(x,y)
   print *, 'compare dprod(xy)=',dd, &
   & 'to x*y=',x*y, &
   & 'to dble(x)*dble(y)=',dble(x)*dble(y)

   print *'test an expected result is produced'
   xx=-6.0d0
   write(*,*)DPROD(-3.0, 2.0),xx
   write(*,*)merge('PASSED','FAILED',DPROD(-3.0, 2.0) == xx)

   print *,'elemental'
   print *, dprod( [2.3,3.4,4.5], 10.0 )
   print *, dprod( [2.3,3.4,4.5], [9.8,7.6,5.4] )

end program demo_dprod
```
Results:
(this can vary between programming environments):
```text
    algebraically 5.2 x 2.3 is exactly 11.96
    as floating point values results may differ slightly:
    compare dprod(xy)=   11.959999313354501      to x*y=   11.9599991
    to dble(x)*dble(y)=   11.959999313354501
    test an expected result is produced
     -6.0000000000000000       -6.0000000000000000
    PASSED
    elemental
      22.999999523162842        34.000000953674316        45.000000000000000
      22.539999971389761        25.840000400543204        24.300000429153442
```
### **Standard**

FORTRAN 77

### **See Also**

[****(3)](#)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
