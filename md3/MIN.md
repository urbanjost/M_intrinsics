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
- The arguments shall all be of the same type and they shall all have the same kind type parameter.
- The type and kind type parameter of the result are the same as those of the arguments.
- For arguments of character type, the length of the result is the length of the longest argument.

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

If arguments are elemental arrays each element of the returned array
shall be the minimum value of the Nth element of all the arrays (or
expanded scalars).

The value of the result is that of the smallest argument. For arguments
of character type, the result is the value that would be selected by
application of intrinsic relational operators; that is, the collating
sequence for characters with the kind type parameter of the arguments is
applied. If the selected argument is shorter than the longest argument,
the result is extended with blanks on the right to the length of the
longest argument.

### **Examples**

Sample program
```fortran
program demo_min
implicit none
integer :: i
integer :: rectangle(3,4)=reshape([(-6+i,i=0,11)],[3,4])
character(len=:),allocatable :: answer
character(len=:),allocatable :: aanswer(:)
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

    write(*,*)'test1 ',merge('PASSED','FAILED', &
    MIN(-9.0, 7.0, 2.0) == -9.0)

    write(*,*)'test2A ',merge('PASSED','FAILED', &
    & MIN('A', 'YY') == 'A ' .and. len(MIN('A','YY')).eq.2)
    write(*,*)'test2B ',merge('PASSED','FAILED', &
    & MIN('AA', 'Y') == 'AA' .and. len(MIN('AA','Y')).eq.2)
    write(*,*)'test2C ',merge('PASSED','FAILED', &
    & MIN('Y', 'AA') == 'AA' .and. len(MIN('Y','AA')).eq.2)
    write(*,*)'test2D ',merge('PASSED','FAILED', &
    & MIN('YY', 'A') == 'A ' .and. len(MIN('YY','A')).eq.2)

    aanswer=MIN(['Z', 'A'], ['YY', 'B '])
    write(*,'(1x,*(g0,1x))') "MIN(['Z', 'A'], ['YY', 'B ']): ",aanswer
    write(*,*)'test3 ',merge('PASSED','FAILED', &
    all(aanswer.eq. ['YY', 'A ']) .and. len(aanswer).eq.2)

end program demo_min
```
Results:
```text
 > basics
 >  -100.000000
 >  -200.000000
 > elemental
 >           1           1           1
 >           2           3           4
 > box:
 > -6  -3   0   3
 > -5  -2   1   4
 > -4  -1   2   5
 > make all values 0 or less:
 > -6  -3   0   0
 > -5  -2   0   0
 > -4  -1   0   0
 > test1 PASSED
 > test2A PASSED
 > test2B PASSED
 > test2C PASSED
 > test2D PASSED
 > MIN(['Z', 'A'], ['YY', 'B ']):  YY A
 > test3 PASSED
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
