## block

### **Name**

**block**(7f) - \[EXECUTION CONTROL\] block construct

### **Synopsis**
Syntax:
```fortran
    [block-construct-name:] BLOCK
    [specification-part]
    ENDBLOCK [block-construct-name]
```
### **Description**

The **block(7f)** construct is an executable construct which may contain
declarations, and may be exited using the **exit(7f)** statement.

Aside from the following restrictions a block construct is in many
ways similar to a contained procedure without parameters accept it is
constructed in-line instead of after the body of the current procedure.

So if you are thinking about making a contained procedure that will be
called once it will probably be clearer inlined using a block construct.

The specification-part of a **block(7f)** construct cannot contain a
**common**, **equivalence**, **implicit**, **intent**, **namelist**,
or **optional** statement.

A **save** of a common-block-name is not allowed in a **block(7f)**
construct.

Except for the **asynchronous** and **volatile** statements,
specifications in a **block(7f)** construct declare construct entities
whose scope is that of the block construct.

### **Examples**

Sample programs:

```fortran
    program demo_block
    implicit none
    integer,parameter :: arr1(*)=[1,2,3,4,5,6,7]
    integer,parameter :: arr2(*)=[0,1,2,3,4,5,6,7]

    ! so when you want error processing to be skipped
    ! if you exhaust a series of tries and really hate GOTO ...
    DEBUG: block
    integer :: icount
       do icount=1,100 ! look for answer up to 100 times
          if(icount.eq.40)exit DEBUG ! found answer, go on
       enddo
       ! never get here unless exhausted the DO loop
       write(*,*)'never found the answer'
       stop 3
    endblock DEBUG
       !
       call showme(arr1)
       call showme(arr2)
       !
    contains
    !
    subroutine showme(a)
    integer,intent(in) :: a(:)
    integer :: i=-100
    integer :: tan
      tan=20 ! intentionally cause a conflict with intrinsic
      ! cannot use tan(3f) right here because using name for a variable
      TESTFORZERO: block
         integer :: I      ! local block variable
         intrinsic :: tan  ! can use the TAN intrinsic in the block now
                           ! as this definition supersedes the one in the
                           ! parent body
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
       write(*,*)'this is the variable back in the main scope, I=',i
    end subroutine showme

    end program demo_block
```
Results:
```text
 >  Never found a zero, tried 7  times
 >  found zero at index 1
 >  this is the variable in the main scope of the program, I=-100
```
### **See Also**

  - [**do**(3)](#do) - construct
  - [**if**(3)](#if) - selects a block based on a sequence of logical expressions.
  - [**cycle**(3)](#cycle) - construct
  - [**exit**(3)](#exit) - statement

  - [**associate**(3)](#associate) - associate construct
  - [**block**(3)](#block) - construct
  - [**goto**(3)](#goto) - jump to target line

  - [**select**(3)](#select) - select a block based on the value of an expression (a case)
  - [**case**(3)](#case) - select a block based on the value of an expression (a case)
  - [**endselect**(3)](#endselect) - select a block based on the value of an expression (a case)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
