## min

### **Name**

**min**(3) - \[NUMERIC\] Minimum value of an argument list

### **Synopsis**
```fortran
    result = min(a1, a2, a3, ... )
```
```fortran
     elemental TYPE(kind=KIND) function min(a1, a2, a3, ... )

      TYPE(kind=KIND,intent(in)   :: a1
      TYPE(kind=KIND,intent(in)   :: a2
      TYPE(kind=KIND,intent(in)   :: a3
                :
                :
                :
```
### **Characteristics**

- **TYPE** may be _integer_, _real_ or _character_.

### **Description**

**min**(3) returns the argument with the smallest (most negative) value.

The arguments must the same type which shall be integer, real,
or character and they also all have the same kind type parameter.

The type and kind type parameter of the result are the same as those
of the arguments.

   NOTE:

A common extension is that the argument kinds can vary. In that case
the returned value may be the kind of the first argument, or might be
the kind of the expression a1+a2+a3+a4... per the rules of promotion.

### **Options**

- **a1**
  : the first element of the set of values to examine.

- **a2, a3, ...**
  : An expression of the same type and kind as **a1** completing the
  set of values to evaluate.

### **Result**

The return value corresponds to the minimum value among the arguments,
and has the same type and kind as the first argument.

### **Examples**

Sample program
```fortran
program demo_min
implicit none
integer :: i
integer :: rectangle(3,4)=reshape([(-6+i,i=0,11)],[3,4])
    print *, 'basics'
    print *, min(10.0,11.0,30.0,-100.0)
    print *, min(-200.0,-1.0)
    print *, 'elemental'
    print *, min(1,[2,3,4])
    print *, min(5,[2,3,4])

    print *, 'box:'
    do i=1,size(rectangle,dim=1)
       write(*,'(*(i3,1x))')rectangle(i,:)
    enddo
    print *, 'make all values 0 or less:'
    do i=1,size(rectangle,dim=1)
       write(*,'(*(i3,1x))')min(rectangle(i,:),0)
    enddo
end program demo_min
```
Results:
```text
 >  basics
 >   -100.000000
 >   -200.000000
 >  elemental
 >            1           1           1
 >            2           3           4
 >  box:
 >  -6  -3   0   3
 >  -5  -2   1   4
 >  -4  -1   2   5
 >  make all values 0 or less:
 >  -6  -3   0   0
 >  -5  -2   0   0
 >  -4  -1   0   0
```
### **Standard**

FORTRAN 77

### **See Also**

[**max**(3)](#max),
[**maxloc**(3)](#maxloc),
[**minloc**(3)](#minloc),
[**minval**(3)](#minval),
[**maxval**(3)](#minval)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
     '
