## where

### **Name**
   where(7f) - [ASSIGNMENT] masked array assignment
           
### **Syntax**
  **where** statement:
```fortran
   WHERE ( mask-expr ) where-assignment-stmt
```
  **where** construct without ELSEWHERE:
```fortran
     [where-construct-name:] WHERE ( mask-expr )
     ELSEWHERE (mask-expr ) [where-construct-name]
```
  **where** construct with ELSEWHEREs:
```fortran
     [where-construct-name:] WHERE ( mask-expr )
     [ELSEWHERE (mask-expr )
        elemental-statements]
     [ELSEWHERE (mask-expr )
        elemental-statements]
           :
     [ELSEWHERE
        elemental-statements
     ]
     END WHERE [where-construct-name]
```
### **Description**
   A masked array assignment is either a **where** statement or a **where**
   construct. It is used to mask the evaluation of expressions and
   assignment of values in array assignment statements, according to
   the value of a logical array expression.

   where-assignment-stmt that is a defined assignment shall be elemental.

   A statement that is part of a where-body-construct shall not be a
   branch target statement.

   If a where-construct contains a where-stmt, a masked-elsewhere-stmt, or
   another where-construct then each mask-expr within the where-construct
   shall have the same shape. In each where-assignment-stmt, the mask-expr
   and the variable being defined shall be arrays of the same shape.

   Examples of a masked array assignment are:

```fortran
            WHERE (TEMP > 100.0) TEMP = TEMP - REDUCE_TEMP

            where (PRESSURE <= 1.0)
               PRESSURE = PRESSURE + INC_PRESSURE
               TEMP = TEMP - 5.0
            elsewhere
               RAINING = .TRUE.
            endwhere
```
   Interpretation of masked array assignments

   When a **where** statement or a where-construct-stmt is executed, a control
   mask is established. In addition,
   when a **where** construct statement is executed, a pending control
   mask is established. If the statement does not appear as part of a
   where-body-construct, the mask-expr of the statement is evaluated,
   and the control mask is established to be the value of mask-expr . The
   pending control mask is established to have the value .NOT. mask-expr
   upon execution of a **where** construct statement that does not appear as
   part of a where-body-construct. The mask-expr is evaluated only once.

   Each statement in a **where** construct is executed in sequence.

   Upon execution of a masked-elsewhere-stmt, the following actions take place in sequence.

   1. The control mask mc is established to have the value of the pending control mask.
   2. The pending control mask is established to have the value mc .AND. (.NOT. mask-expr ).
   3. The control mask mc is established to have the value mc .AND. mask-expr .

   The mask-expr is evaluated at most once.

   Upon execution of an **elsewhere** statement, the control mask is
   established to have the value of the pending
   control mask. No new pending control mask value is established.

   Upon execution of an **endwhere** statement, the control mask and pending
   control mask are established to have the values they had prior to the
   execution of the corresponding **where** construct statement. Following the
   execution of a **where** statement that appears as a where-body-construct,
   the control mask is established to have the value it had prior to the
   execution of the **where** statement.

   The establishment of control masks and the pending control mask
   is illustrated with the following example:

```fortran
            where(cond1)             ! Statement 1
            . . .
            elsewhere(cond2)         ! Statement 2
            . . .
            elsewhere                ! Statement 3
            . . .
            end where
```

   Following execution of statement 1, the control mask has the value
   cond1 and the pending control mask has the value .NOT. cond1.
   Following execution of statement 2, the control mask has the value
   (.NOT. cond1) .AND. cond2 and the pending control mask has the
   value (.NOT. cond1) .AND. (.NOT. cond2). Following execution
   of statement 3, the control mask has the value (.NOT. cond1)
   .AND. (.NOT. cond2). The false condition values are propagated
   through the execution of the masked **elsewhere** statement.

   Upon execution of a **where** construct statement that is part of a
   where-body-construct, the pending control mask is established to
   have the value mc .AND. (.NOT. mask-expr ). The control mask is then
   established to have the value mc .AND. mask-expr. The mask-expr is
   evaluated at most once.

   Upon execution of a **where** statement that is part of a
   where-body-construct, the control mask is established to have the
   value mc .AND. mask-expr. The pending control mask is not altered.

   If a nonelemental function reference occurs in the expr or variable
   of a where-assignment-stmt or in a mask-expr ,
   the function is evaluated without any masked control; that is, all of
   its argument expressions are fully evaluated and the function is fully
   evaluated. If the result is an array and the reference is not within
   the argument list of a nonelemental function, elements corresponding
   to true values in the control mask are selected for use in evaluating
   the expr, variable or mask-expr.

   If an elemental operation or function reference occurs in the expr
   or variable of a where-assignment-stmt or in a
   mask-expr , and is not within the argument list of a nonelemental
   function reference, the operation is performed or the function is
   evaluated only for the elements corresponding to true values of the
   control mask.

   If an array constructor appears in a where-assignment-stmt or in a
   mask-expr , the array constructor is evaluated
   without any masked control and then the where-assignment-stmt is
   executed or the mask-expr is evaluated.

   When a where-assignment-stmt is executed, the values of expr that
   correspond to true values of the control mask
   are assigned to the corresponding elements of the variable.

   The value of the control mask is established by the execution of a
   **where** statement, a **where** construct
   statement, an ELSEWHERE statement, a masked ELSEWHERE statement, or
   an ENDWHERE statement. Subsequent changes to the value of entities
   in a mask-expr have no effect on the value of the control mask. The
   execution of a function reference in the mask expression of a **where**
   statement is permitted to affect entities in the assignment statement.

   Examples of function references in masked array assignments are:

```fortran
      where (A > 0.0)
         A = LOG (A)           ! LOG is invoked only for positive elements.
         A = A / SUM (LOG (A)) ! LOG is invoked for all elements
                               ! because SUM is transformational.
      end where
```

### **Example**

  Sample

```fortran
   program demo_where
   !  Example of **where**, ELSE **where**, END **where**
   integer,parameter :: nd=10, ndh=nd/2, nduh=nd-ndh-1
   integer :: j
   real, dimension(nd):: a=[ (2*j,j=1,nd) ]
   real, dimension(nd):: b ! =[ ndh*1.0, 0.0, nduh*2.0 ]
   real, dimension(nd):: c ! =[ nd*-77.77 ]
   integer iflag(nd)
   data b/ndh*1,0.0,nduh*2./,c/nd*-77.77/

   where (b.ne.0) c=a/b
   write (*,2000) 'a ...',a(1:nd)
   write (*,2000) 'b ...',b(1:nd)
   write (*,2000) 'c ...',c(1:nd)
   !
   !  The above protects against divide by zero, but doesn't actually assign
   !  values to elements in c when the corresponding element in b is zero
   !  The following covers that, and sets a flag when a divide by zero is
   !  present
   !
   where (b(1:nd).ne.0.0)
      c=a/b
      iflag=0
   else where
      c=0.0
      iflag=1
   end where

   write (*,2000) 'c=a/b ...',c(1:nd)
   write (*,1000) iflag(1:nd)
   1000 format ('iflag= ',/,(10i7))
   2000 format (a,/,(10f7.2))
   end program demo_where
```
Results:
```text
 > a ...
 >    2.00   4.00   6.00   8.00  10.00  12.00  14.00  16.00  18.00  20.00
 > b ...
 >    1.00   1.00   1.00   1.00   1.00   0.00   2.00   2.00   2.00   2.00
 > c ...
 >    2.00   4.00   6.00   8.00  10.00 -77.77   7.00   8.00   9.00  10.00
 > c=a/b ...
 >    2.00   4.00   6.00   8.00  10.00   0.00   7.00   8.00   9.00  10.00
 > iflag= 
 >       0      0      0      0      0      1      0      0      0      0
```
