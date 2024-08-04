### **Name**

**block**(7f) - \[EXECUTION CONTROL\] block construct

### **Synopsis**
```fortran
   [block-construct-name:] BLOCK
   [specification-part]
   ENDBLOCK [block-construct-name]
```
### **Description**

The *block(7f)* construct is an executable construct which may contain
declarations, and may be exited using the **exit(7f)** statement.

The specification-part of a *block(7f)* construct cannot contain a
*common*, *equivalence*, *implicit*, *intent*, *namelist*, or *optional*
statement.

A **save** of a common-block-name is not allowed in a *block(7f)*
construct.

Aside from these restrictions a block construct is in many ways similiar
to a contained procedure without parameters accept it is constructed
in-line instead of after the body of the current procedure.

Except for the **asynchronous** and **volatile** statements,
specifications in a *block(7f)* construct declare construct entities
whose scope is that of the block construct.

### **Examples**

Sample programs:

```fortran
   program demo_block
   implicit none
   integer,parameter :: arr1(*)=[1,2,3,4,5,6,7]
   integer,parameter :: arr2(*)=[0,1,2,3,4,5,6,7]
     call showme(arr1)
     call showme(arr2)
     contains
   subroutine showme(a)
   integer,intent(in) :: a(:)
   integer :: i=-100
   integer :: tan
     tan=20 ! intentionally cause a conflict with intrinsic
     TESTFORZERO: block
        integer :: I      ! local block variable
        intrinsic :: tan  ! can use the TAN intrinsic in the block
        do i=1,size(a)
           if(a(i).eq.0) then
              write(*,*)'found zero at index',i
              exit TESTFORZERO
           endif
        enddo
        write(*,*)'Never found a zero, tried ',i-1,' times'
        return
      endblock TESTFORZERO
      ! note the variable I in the block is local to the block
      write(*,*)'this is the variable in the main scope of the program, I=',i
   end subroutine showme

   end program demo_block
```
Results:
```text
 >  Never found a zero, tried 7  times
 >  found zero at index 1
 >  this is the variable in the main scope of the program, I=-100
```
 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
