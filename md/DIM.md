## dim

### **Name**

**dim**(3) - \[NUMERIC\] Positive difference

### **Syntax**
```fortran
    result = dim(x, y)
```
```fortran
     elemental type(TYPE(kind=KIND)) function dim(x, y)

     type(TYPE(kind=KIND)),intent(in) :: x, y
```
where TYPE may be _real_ or _integer_ and KIND is any supported kind
for the type.

### **Description**

**dim(x,y)** returns the difference **x - y** if the result is positive;
otherwise it returns zero. It is equivalent to **max(0,x-y)** where the
arguments are all of the same type.

### **Arguments**

- **x**
  : The type shall be _integer_ or _real_

- **y**
  : The type shall be the same type and kind as **x**.

### **Returns**

Returns the difference **xx -y** or zero, whichever is larger.
The return value is the same type and kind as the input arguments **x**
and **y**.

### **Examples**

Sample program:

```fortran
program demo_dim
use, intrinsic :: iso_fortran_env, only : real64
implicit none
integer           :: i
real(kind=real64) :: x

   ! basic usage
    i = dim(4, 15)
    x = dim(4.321_real64, 1.111_real64)
    print *, i
    print *, x

   ! elemental
    print *, dim([1,2,3],2)
    print *, dim([1,2,3],[3,2,1])
    print *, dim(-10,[0,-10,-20])

end program demo_dim
```

Results:

```text
              0
      3.21000000000000
              0           0           1
              0           0           2
              0           0          10
```

### **Standard**

FORTRAN 77 and later

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
