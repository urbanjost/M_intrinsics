## cmplx

### **Name**

**cmplx**(3) - \[TYPE:NUMERIC\] Complex conversion function

### **Synopsis**
```fortran
    result = cmplx(x [,y] [,kind])
```
```fortran
      elemental complex function cmplx( x, y, kind )

      TYPE(kind=KIND),intent(in) :: x
      TYPE(kind=KIND),intent(in),optional :: y
      integer,intent(in),optional :: kind
```
### **Characteristics**

### **Description**

To convert numeric variables to complex, use the **cmplx**(3) function.
Constants can be used to define a complex variable using the syntax

```
      z8 = (1.2345678901234567d0, 1.2345678901234567d0)
```

but this will not work for variables. You must use the **cmplx**(3) function.

**cmplx(x \[, y \[, kind\]\])** returns a complex number where **x** is
converted to the _real_ component. If **x** is _complex_ then **y** must not be
present. If **y** is present it is converted to the imaginary component. If
**y** is not present then the imaginary component is set to **0.0**.

### **cmplx(3) and double precision**

The Fortran 90 language defines **cmplx**(3) as always returning a result
that is type **complex(kind=KIND(0.0))**.

This means **cmplx(d1,d2)**, where **d1** and **d2** are
_doubleprecision_, is treated as:
```fortran
      cmplx(sngl(d1), sngl(d2))
```
_doubleprecision complex_ numbers require specifying a precision.

It was necessary for Fortran 90 to specify this behavior for
_doubleprecision_ arguments, since that is the behavior mandated by
FORTRAN 77.

So Fortran 90 extends the **cmplx**(3) intrinsic by adding an extra
argument used to specify the desired kind of complex result.

```fortran
      integer,parameter :: dp=kind(0.0d0)
      complex(kind=dp) :: z8
      !
      ! NO: result is just the precision of default real values
      !     because KIND parameter is not specified
      !
      ! note this was stored with default real precision
      z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0)
      print *, 'NO, Z8=',z8,real(z8),aimag(z8)
      z8 = cmplx(1.2345678901234567e0_dp, 1.2345678901234567e0_dp)
      ! again, note components are just real
      print *, 'NO, Z8=',z8,real(z8),aimag(z8)
      !
      ! YES
      !
      ! kind= makes it work
      z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0,kind=dp)
      print *, 'YES, Z8=',z8,real(z8),aimag(z8)
```

F2018 COMPONENT SYNTAX The real and imaginary parts of a complex entity
can be accessed independently with a component-like syntax in f2018:

A complex-part-designator is

```fortran
designator % RE
or
designator % IM.

````

Where the designator is of complex type.

So designator%RE designates the real part of a complex value,
designator%IM designates the imaginary part of complex value. The type
of a complex-part-designator is _real_, and its kind and shape are those
of the designator.

The following are examples of complex part designators:

```fortran
       impedance%re           !-- Same value as _real_(impedance)
       fft%im                 !-- Same value as AIMAG(fft)
       x%im = 0.0             !-- Sets the imaginary part of x to zero
````

### **Options**

- **x**
  The type may be _integer_, _real_, or _complex_.

- **y**
  (Optional; only allowed if **x** is not _complex_.). May be _integer_ or
  _real_.

- **kind**
  (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Result**

The return value is of _complex_ type, with a kind equal to **kind** if it is
specified. If **kind** is not specified, the result is of the default
_complex_ kind, regardless of the kinds of **x** and **y**.

### **Examples**

Sample program:

```fortran
program demo_aimag
implicit none
integer,parameter :: dp=kind(0.0d0)
complex          :: z4
complex(kind=dp) :: z8
   z4 = cmplx(1.23456789, 1.23456789)
   print *, 'Z4=',z4
   ! using kind=dp makes it keep DOUBLEPRECISION precision
   z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0,kind=dp)
   print *, 'Z8=',z8
   ! NOTE:
   ! The following is intuitive and works without calling cmplx(3)
   ! but does not work for variables just constants
   z8 = (1.2345678901234567d0, 1.2345678901234567d0 )
   print *, 'Z8 defined with constants=',z8
end program demo_aimag
```

Typical Results:

```
    Z4= (1.23456788,1.23456788)
    Z8= (1.2345678901234567,1.2345678901234567)
    Z8 defined with constants= (1.2345678901234567,1.2345678901234567)
```
### **Standard**

FORTRAN 77

### **See Also**

- [**aimag**(3)](#aimag) - Imaginary part of complex number
- [**conjg**(3)](#conjg) - Complex conjugate function
- [**real**(3)](#real) - Convert to real type

Fortran has strong support for _complex_ values, including many intrinsics
that take or produce _complex_ values in addition to algebraic and
logical expressions:

[**abs**(3)](#abs),
[**acosh**(3)](#acosh),
[**acos**(3)](#acos),
[**asinh**(3)](#asinh),
[**asin**(3)](#asin),
[**atan2**(3)](#atan2),
[**atanh**(3)](#atanh),
[**atan**(3)](#atan),
[**cosh**(3)](#cosh),
[**cos**(3)](#cos),
[**co_sum**(3)](#co_sum),
[**dble**(3)](#dble),
[**dot_product**(3)](#dot_product),
[**exp**(3)](#exp),
[**int**(3)](#int),
[**is_contiguous**(3)](#is_contiguous),
[**kind**(3)](#kind),
[**log**(3)](#log),
[**matmul**(3)](#matmul),
[**precision**(3)](#precision),
[**product**(3)](#product),
[**range**(3)](#range),
[**rank**(3)](#rank),
[**sinh**(3)](#sinh),
[**sin**(3)](#sin),
[**sqrt**(3)](#sqrt),
[**storage_size**(3)](#storage_size),
[**sum**(3)](#sum),
[**tanh**(3)](#tanh),
[**tan**(3)](#tan),
[**unpack**(3)](#unpack),

 _fortran-lang intrinsic descriptions_
