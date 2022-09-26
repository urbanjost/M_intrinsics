## present

### **Name**

**present**(3) - [STATE\] Determine whether an optional dummy argument
is specified

### **Synopsis**
```fortran
    result = present(a)
```
```fortran
     logical function present (a)
     type(TYPE(kind=KIND)) :: a(..)
```
where the **TYPE** may be any type
### **Description**

Determines whether an optional dummy argument is present.

### **Options**

- **a**
  : May be of any type and may be a pointer, scalar or array value,
  or a dummy procedure. It shall be the name of an optional dummy
  argument accessible within the current subroutine or function.

### **Result**

Returns either _.true._ if the optional argument **a** is present,
or _.false._ otherwise.

### **Examples**

Sample program:

```fortran
program demo_present
implicit none
   write(*,*) func(), func(42)
contains

integer function func(x)
integer, intent(in), optional :: x
   if(present(x))then
     func=x**2
   else
     func=0
   endif
end function

end program demo_present
```

Results:

```text
     0        1764
```

### **Standard**

Fortran 95 and later

 _fortran-lang intrinsic descriptions_
