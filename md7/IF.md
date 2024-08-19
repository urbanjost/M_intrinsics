## if

### **Name**
     if(7f) - [EXECUTION CONTROL] selects a block based on a
     sequence of logical expressions.

### **Synopsis**
Syntax:
```fortran
     [if_construct_name:] IF (scalar-logical-expr) THEN
        block
     ELSEIF (scalar-logical-expr) THEN [if_construct_name]
        block
     ELSE [if_construct_name]
        block
     ENDIF [if_construct_name]
```
        or
```fortran
     IF (scalar-logical-expression) action-statement
```
### **Description**

   The **if** construct selects for execution at most one of its constituent
   blocks. The selection is based on a sequence of logical expressions.

   If an if-construct-name is specified, both the **if** and **endif** must use
   that same name. If an **else** or **elseif** uses an if-construct-name it must
   be the same as the one specified on the corresponding **IF**/**ENDIF**.

   **Execution of an IF Construct**

   If there is an **else** statement in the construct it acts as a default
   if all the preceding conditionals on the **if** or **elseif** where false,
   ensuring exactly one of the blocks in the construct is executed. The
   scalar logical expressions are evaluated in the order of their
   appearance in the construct until a true value is found or an **else**
   statement or **endif** statement is encountered. **if** an **else** statement is
   found, the block immediately following is executed and this completes
   the execution of the construct. That is, an **else** should follow any
   **elseif** statements.  The scalar logical expressions in any remaining
   **elseif** statements of the **IF** construct are not evaluated. If none of
   the evaluated expressions is true and there is no **else** statement,
   the execution of the construct is completed without the execution of
   any block within the construct.

   It is permissible to branch to an **endif** statement only from within
   its IF construct. Execution of an **endif** statement has no effect.

   **Standalone IF**

   The **if** statement controls the execution of a single action
   statement based on a single logical expression.

   The action-stmt in the if-stmt shall not be an end-function-stmt,
   end-mp-subprogram-stmt, end-program-stmt, end-subroutine-stmt,
   or if-stmt.

   Execution of an **if** statement causes evaluation of the scalar
   logical expression. If the value of the expression is true, the action
   statement is executed. If the value is false, the action statement
   is not executed and execution continues.

   The execution of a function reference in the scalar logical expression
   may affect entities in the action statement. That is, if values
   are changed by the functions used in the logical expressions the
   selected block will use those values. It is generally a bad idea to
   use functions changing the values, but what would you expect this
   to produce?
```fortran
   Calling a function with side-effects on **i**;
        program change
        i=1
        if(increment(i).gt.10)then
           write(*,*)'IF',i
        elseif(increment(i).ge.20)then
           write(*,*)'ELSEIF',i
        else
           write(*,*)'ELSE',i
        endif
        contains
        function increment(i)
           write(*,*)'INC',i
           increment=i*5
           i=i+3
           write(*,*)'INC',i
        end function increment
        end program change
```
   Result:
```text
     > INC           1
     > INC           4
     > INC           4
     > INC           7
     > ELSEIF           7
```
   An example of an IF statement is:
```fortran
       IF (A > 0.0) A = LOG (A)
```
### **Examples**

  Sample IF constructs:
```fortran
    program demo_if
    implicit none
    character(len=:),allocatable :: cvar
    logical :: PROP=.false.
    real :: a, b, c, d
    integer :: case=0
    integer :: i, j, k
    logical :: nextprop=.true.
     !
     ! basic IF
     !
     cvar='NO'
     if (cvar == 'RESET') then
        i = 0; j = 0; k = 0
     endif
     !
     ! labeled and nested IF constructs
     !
     OUTER: if (case.eq.0)then
        PROOF_DONE: if (PROP) then
           write (3, '(''QED'')')
           exit OUTER
        else
           PROP = nextprop
        endif PROOF_DONE
        write(*,*)'END OF PROOF_DONE'
     else OUTER
             write(*,*)'else outer'
     endif OUTER
     !
     ! if-elseif-endif
     !
     if (a > 0) then
        b = c/a
        if (b > 0) then
           d = 1.0
        endif
     elseif (c > 0) then
        b = a/c
        d = -1.0
     else
        b = abs (max (a, c))
        d = 0
     endif
     !
    end program demo_if
```
### **See Also**

  - do  - construct
  - if  - selects a block based on a sequence of logical expressions.
  - cycle  - construct
  - exit  - statement

  - associate  - associate construct
  - block  - construct
  - goto  - jump to target line

  - select  - select a block based on the value of an expression (a case)
  - case  - select a block based on the value of an expression (a case)
  - endselect  - select a block based on the value of an expression (a case)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
