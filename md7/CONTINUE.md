## continue

### **Name**

**continue**(7) - \[EXECUTION\_CONTROL\] execution of a CONTINUE statement has no effect

### **Synopsis**
```fortran
[NNNNN] continue
```
### **Description**

It is generally very confusing to have executable statements on labeled
lines; a **continue** statement eliminates the ambiguities that arise
in jumping to an executable line. Specifically:

  + Execution of a **continue** statement has no effect.
  + Preferably no target of a transfer should be an executable statement.
  + Therefore, all numerically labeled executable lines should be a **continue**.

A **continue** statement is most often used as a target for transfer
control statements such as **goto**. That is,  a numeric label is added
to the line.

CONTINUE(7)  is rarely used in new code but was very commonly encountered
in older FORTRAN code before the advent of constructs like **enddo**,
**cycle**, **block**, and **exit**.

### **Examples**

Sample program:
```fortran
>      program oldstyle
>      integer i,j
>      j=5
>      do 100 i=1,20
>         if(i.lt.5)goto 100
>         j=3
>100   write(*,*)'J=',j
>      end
```
```fortran
program demo_continue
! numbered targets should (almost?) always be a continue statement
! with a unique label for each looping structure
integer :: i,j
  j=5
  do 100 i=1,20
     if(i.lt.5)goto 50
     j=3
     50 continue
     write(*,*)'J=',j
  100 continue
end program demo_continue
```
```fortran
program newer
implicit none
integer :: i,j
   j=5
   do i=1,20
      if(i >= 5)then
         j=3
      endif
      write(*,*)'J=',j
   enddo
end program newer
```
 _Fortran statement descriptions (license: MIT) \@urbanjost_
