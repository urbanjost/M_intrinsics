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
control statements such as **goto**. by adding a numeric label to
the line.

It is rarely used in new code but was very commonly encountered in older
FORTRAN code before the advent of constructs like **enddo**.

### **Examples**

Sample program:
```fortran
   program oldstyle
   integer :: i,j
         j=5
         do 100 i=1,20
            if(i.lt.5)goto 100
            j=3
   100   write(*,*)'J=',j
   end

   program better
   integer :: i,j
         j=5
         do 100 i=1,20
            if(i.lt.5)goto 50 
               j=3
   50       continue
            write(*,*)'J=',j
   100   continue
   end

   program newer
   implicit none
   integer :: i,j
   j=5
   do i=1,20
      if(i.ge.5)then
         j=3
      endif   
      write(*,*)'J=',j
   enddo   
   end program newer
```
 _fortran-lang statement descriptions (license: MIT) \@urbanjost_
