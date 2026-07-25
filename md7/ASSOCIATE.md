## associate

### **Name**

**associate**(7) - \[EXECUTION CONTROL\] aliases selected variable
identifiers as well as creates constants within the block from runtime
expressions at entry into the block
(LICENSE:PD)

### **Syntax**
Syntax:
```fortran
  [ construct-name : ] ASSOCIATE ( associate-name => selector ...)
    :
    : the associate-block; zero or more statements or constructs
    :
  END ASSOCIATE [ construct-name ]
```
For example:
```fortran
   ! pi is an associate-name, acos(-1.0) is its selector
   associate (pi => acos(-1.0) )
      print *, pi
   end associate
```
    ASSOCIATE-NAME

     An identifier that becomes associated with the selector within the
     ASSOCIATE block. The "associate-name" must be unique within the
     construct (but the name can be redefined in other nested subblocks
     such as BLOCK constructs and additional ASSOCIATE blocks).

    SELECTOR

     Is an expression or variable that will be referred to by the
     associate-name.

    CONSTRUCT-NAME

     An optional name for the block. It is generally used so the block
     can be exited by name or to distinguish which end statement
     is connected with which ASSOCIATE statement when blocks are heavily
     nested.

     If a construct-name is specified the same name must appear at both
     the beginning of the block in the ASSOCIATE statement and at the
     end in the END ASSOCIATE statement.

     Construct names must be unique in the scoping unit. Once used that
     same construct-name must not be used for any other named constructs
     in the same scoping unit.

     It is permissible to branch to an end-associate-stmt only from
     within its ASSOCIATE construct.

```fortran
        MYNAME: associate
         :
        if(something_is_true) exit MYNAME
         :
        end associate MYNAME
```
 sample program:

```fortran
   program show_exiting
   implicit none
   integer :: values(8)

   call date_and_time( values=values )

   CALENDAR: associate ( cal=values([1,2,3,5,6,7,4]), &
      year                        =>  VALUES(1), &
      month                       =>  VALUES(2), &
      day                         =>  VALUES(3), &
      delta_from_UTC_in_minutes   =>  VALUES(4), &
      hour_of_day                 =>  VALUES(5), &
      minutes_of_the_hour         =>  VALUES(6), &
      seconds_of_the_minute       =>  VALUES(7), &
      milliseconds_of_the_second  =>  VALUES(8) )
      if(hour_of_day.lt.6)  exit CALENDAR
      if(hour_of_day.ge.18) exit CALENDAR
      write(*,'(i4.4,"-",i2.2,"-",i2.2,"T", &
      & i2.2,":",i2.2,":",i2.2,sp,i0.4)')cal
   end associate CALENDAR

   end program show_exiting
```
### **Description**
An ASSOCIATE statement can rename syntactically complex data selectors
with simple or more descriptive aliases and also allows for simple names
to be used for the value of expressions at the top of the block (this
value becomes a constant in the code block). ASSOCIATE is thus used to
make complicated expressions more readable and maintainable by developers.

The ASSOCIATE construct creates a temporary association between a
identifier and a variable or the value of an expression. The association
lasts for the duration of the block.

Each ASSOCIATE statement must be followed by a matching END ASSOCIATE

The variable will have most, but not all of the attributes of the
variable.

More specifically an ASSOCIATE statement either

1. creates a name for a constant in the block using the value of an
   expression defined in the ASSOCIATE statement.

   or

2. creates an alias for a long variable name. When the RHS is a variable
   changing the alias value changes the associated name outside
   of the block as well.

   If the selector of an ASSOCIATE is a variable, the associate-name
   can be changed in an ordinary assignment, which also changes the
   variable.

An alias for a variable is useful when you want to simplify multiple
accesses to a variable that has a lengthy description. An example would be
if the initial variable contains multiple subscripts and component names.

On the other hand an expression (instead of a variable) on the RHS
becomes a named constant in the block.

The ASSOCIATE statement is _NOT_ equivalent to a function statement or
a macro. That would generally be implemented via a contained procedure.

  o If the selector is an expression or a variable that
    has a vector subscript, the associate-name shall not appear in a
    variable definition context but will behave as a constant, much
    like a parameter of a procedure with INTENT(IN). That is, the
    associate-name cannot be changed in an ordinary assignment.

    Note: A vector subscript is an integer array expression of rank one,
          designating a sequence of subscripts that correspond to the
          values of the elements of the expression.

          The sequence does not have to be in order, and may contain
          duplicate values:

             INTEGER A(10), B(3)
             ! B(1) = A(1); B(2) = A(2); B(3) = A(2) also
             B = A( [1,2,2] )

  o An associate-name shall not be the same as another
    associate-name in the same associate-stmt.

  o The variable name on the RHS shall not be a coindexed object.

  o expr shall not be a variable. Note putting a variable in parentheses
    makes it an expression.

### **Execution of the Associate Construct**

  Execution of an ASSOCIATE construct causes evaluation of every
  expression used as a selector, followed by execution of its block.

  During execution of the block within the construct, each associate-name
  identifies an entity associated with the corresponding selector.

  The associating entity assumes the declared type and type parameters
  of the selector.

  If and only if the selector is polymorphic, the associating entity
  is polymorphic.

### **Attributes of Associate Names**

   Within an ASSOCIATE or SELECT TYPE construct, each associating entity
   has the same rank and corank as its associated selector.

   The lower bound of each dimension is the result of the intrinsic
   function LBOUND(3) applied to the corresponding dimension of selector.
   The upper bound of each dimension is one less than the sum of the
   lower bound and the extent.

#### Sample showing affects on custom bounds:
```fortran
   program show_bounds
   implicit none
   character(len=*),parameter :: & ! a format
   & bounds="('bounds of ',a,'=>(',i0,':',i0,',',i0,':',i0,')')"
   integer :: arr(-5:5,-5:5) ! custom non-normal bounds
   integer :: b(4)
     ! first the different between queries of arr versus arr(:,:)
      b([1,3,2,4])=[lbound(arr),ubound(arr)]
      print bounds,'arr', b
      b([1,3,2,4])=[lbound(arr(:,:)),ubound(arr(:,:))]
      print bounds,'arr(:,:)',b
     !
     ! and the bounds assigned to the identifiers are what UBOUND(3)
     ! and LBOUND(3) return given the selector as an argument so
      associate ( &
         alias=>   arr,       & ! keeps the custom bounds
         normal=>  arr(:,:)   & ! gets normal bounds
         )
         b([1,3,2,4])=[lbound(alias),ubound(alias)]
         print bounds,'alias', b
         b([1,3,2,4])=[lbound(normal),ubound(normal)]
         print bounds,'normal',b
      end associate
   end program show_bounds
```
Results:
```text
      bounds of arr=>(-5:5,-5:5)
      bounds of arr(:,:)=>(1:11,1:11)
      bounds of alias=>(-5:5,-5:5)
      bounds of normal=>(1:11,1:11)
```
   The cobounds of each codimension of the associating entity are the same
   as those of the selector.

   The associating entity has the ASYNCHRONOUS
   or VOLATILE attribute if and only if the selector is a variable and
   has the attribute.

   The associating entity has the TARGET attribute if
   and only if the selector is a variable and has either the TARGET or
   POINTER attribute.

   The selector must be allocated if allocatable.
   The associate-name is not ALLOCATABLE even if the selector is.

   If a selector has the POINTER attribute, it shall be associated.
   The associate name is associated with the target of the pointer and
   does not have the POINTER attribute.

   If the associating entity is polymorphic, it assumes the dynamic type
   and type parameter values of the selector.

   If the selector has the OPTIONAL attribute, it shall be present (It
   cannot be absent). The associating entity does not have the OPTIONAL
   attribute.

   The associating entity is contiguous if and only if the selector
   is contiguous.

   If the selector is not permitted to appear in a variable definition
   context, the associate name shall not appear in a variable
   definition context.

   The selector has the TARGET attribute if and only if the selector is
   a variable and has either the TARGET or POINTER attribute.

   expr shall not be a designator of a procedure pointer or a function
   reference that returns a procedure pointer.

   Within an ASSOCIATE construct, each associating entity has the same
   corank as its associated selector. If the selector is a coarray,
   the cobounds of each codimension of the associating entity are the
   same as those of the selector.

   The associating entity itself is a variable, but if the selector is
   not a definable variable, the associating entity is not definable
   and shall not be defined or become undefined.

   If a selector is not permitted to appear in a variable definition
   context, neither the associate name nor any subobject thereof shall
   appear in a variable definition context or pointer association context.

### **nesting**

No other block may be created in an ASSOCIATE block that is not terminated
in the block; and the ASSOCIATE block must be terminated in the block
it was created in. For example, if an ASSOCIATE block is begun in a DO
loop it must be terminated before the end of the loop. Conversely if a
DO loop is created in an ASSOCIATE block it must be terminated before the
end of the ASSOCIATE block.

An associate-name can appear in an ASSOCIATE statement even if it
previously appeared in an ASSOCIATE statement that has not been terminated.

### **Samples**
  The following shows association with an array section:
```fortran
   associate (array => ab % d(i, :) % x)
     array(3) = array(1) + array(2)
   end associate
```
  instead of the equivalent statement
```fortran
   ab % d(i,3) % x = ab % d(i,1) % x + ab % d(i,2) % x
```
  This example illustrates an association with an expression.
```fortran
     associate ( z => exp(-(x**2+y**2)) * cos(theta) )
         ! creates the constant "z"
         print *, a+z, a-z
     end associate
```
  an association with a derived-type variable:
```fortran
       associate ( xc => ax%b(i,j)%c )
         xc%dv = xc%dv + product(xc%ev(1:n))
       end associate
```
  association with an array section:
```fortran
       associate ( quadrantIII =>  array(1:5,6:10) )
         quadrantIII = 0
       end associate
```
  The next example illustrates multiple associations.
```fortran
       associate ( w => result(i,j)%w, &
       & zx => ax%b(i,j)%d, &
       & zy => ay%b(i,j)%d )
         w = zx*x + zy*y
       end associate
```
  An ASSOCIATE block may not span other block boundaries
```fortran
do i=1,3
   associate (x => real(i)) !since this was started inside the DO loop
      print*,i,sqrt(x)
   end associate ! the end must appear before the end of the DO loop
enddo
```
### **Examples**
Sample program:

```fortran
   program demo_associate
   implicit none
   character(len=*),parameter :: g='(*(g0,1x))'
   character :: array(-5:5,-5:5)      ! custom non-normal bounds
   ! note the different between queries of ARRAY versus ARRAY(:,:)
     write(*,g)'array:     ',  'lbound=',lbound(array), &
                               'ubound=',ubound(array)
     write(*,g)'array(:,:): ', 'lbound=',lbound(array(:,:)), &
                               'ubound=',ubound(array(:,:))
   ! the bounds assigned to the identifiers are what UBOUND(3)
   ! and LBOUND(3) return given the selector as an argument
     associate ( &
      alias=>   array,              & ! keeps the custom bounds
      normal=>  array(:,:),         & ! gets normal bounds
      quadI=>   array(+1:+5,-5:-1), & ! quad* will have normal bounds
      quadII=>  array(-5:-1,-5:-1), & !
      quadIII=> array(-5:-1,+1:+5), & !
      quadIV=>  array(+1:+5,+1:+5), & !
      xaxis=>array(:,0), &
      yaxis=>array(0,:) &
      )
      array='.' ! selector name is still valid in the block
      xaxis='-'
      yaxis='|'
      alias(0,0)='+' ! uses non-normal bounds, equivalent to array(0,0)='+'
      write(*,'(11(g0,1x))') alias
      ! the quads have normalized dimension bounds (1:5,1:5):
      quadI    =  '1';  quadI(1,1)    =  'a';  quadI(5,5)    =  'A'
      quadII   =  '2';  quadII(1,1)   =  'b';  quadII(5,5)   =  'B'
      quadIII  =  '3';  quadIII(1,1)  =  'c';  quadIII(5,5)  =  'C'
      quadIV   =  '4';  quadIV(1,1)   =  'd';  quadIV(5,5)   =  'D'
      write(*,'(11(g0,1x))') alias
      write(*,g)'array:  lbound=',lbound(array), 'ubound=',ubound(array)
      write(*,g)'alias:  lbound=',lbound(alias), 'ubound=',ubound(alias)
      write(*,g)'normal: lbound=',lbound(normal),'ubound=',ubound(normal)
      write(*,g)'quadI:  lbound=',lbound(quadI), 'ubound=',ubound(quadI)
      write(*,g)'quadII: lbound=',lbound(quadII),'ubound=',ubound(quadII)
      write(*,g)'quadIV: lbound=',lbound(quadIV),'ubound=',ubound(quadIV)
     end associate
   end program demo_associate
```
Results:
```text
      array:      lbound= -5 -5 ubound= 5 5
      array(:,:):  lbound= 1 1 ubound= 11 11
      . . . . . | . . . . .
      . . . . . | . . . . .
      . . . . . | . . . . .
      . . . . . | . . . . .
      . . . . . | . . . . .
      - - - - - + - - - - -
      . . . . . | . . . . .
      . . . . . | . . . . .
      . . . . . | . . . . .
      . . . . . | . . . . .
      . . . . . | . . . . .
      b 2 2 2 2 | a 1 1 1 1
      2 2 2 2 2 | 1 1 1 1 1
      2 2 2 2 2 | 1 1 1 1 1
      2 2 2 2 2 | 1 1 1 1 1
      2 2 2 2 B | 1 1 1 1 A
      - - - - - + - - - - -
      c 3 3 3 3 | d 4 4 4 4
      3 3 3 3 3 | 4 4 4 4 4
      3 3 3 3 3 | 4 4 4 4 4
      3 3 3 3 3 | 4 4 4 4 4
      3 3 3 3 C | 4 4 4 4 D
      array:   lbound= -5 -5 ubound= 5 5
      alias:   lbound= -5 -5 ubound= 5 5
      normal:  lbound= 1 1 ubound= 11 11
      quadI:   lbound= 1 1 ubound= 5 5
      quadII:  lbound= 1 1 ubound= 5 5
      quadIII: lbound= 1 1 ubound= 5 5
      quadIV:  lbound= 1 1 ubound= 5 5
```
### Dusty Corners

If the expressions have side-effects are they executed only when the
block is entered?

Selected variable names are still accessible in the ASSOCIATE block. This
is confusing and should be avoided, particular if the selectors
are allocatable or pointers. This is similar to variables passed as
arguments to contained procedures but referenced via the argument name
and the name in the surrounding scope. The behavior is ill-defined. Does
a change to the argument take affect immediately or upon return from the
procedure? If the argument is not declared allocatable or is a pointer
does the argument name value get changed by deallocation or disassociation
or changes to the original names?

are you allowed to allocate v to a different size before the ASSOCIATE
is terminated? If so, what happens to c ?

Does that mean it is invalid to resize v within the ASSOCIATE block? Or
is it only invalid to resize v and then refer to c? Or only invalid to
resize v and refer to c when c is associated with elements of v that no
longer exist?
```fortran
   implicit none
   integer, allocatable, target :: v(:)
   integer, pointer :: p(:)
      v = [4,7,9]
      p => v
      print*,p
      deallocate(v)
      print*,p ! invalid, because target has been deallocated
   end program main
```
are you allowed to allocate v to a different size before the ASSOCIATE
is terminated? If so, what happens to c?
```fortran
     program demonstrate_associate
     implicit none
     integer, allocatable :: v(:)
     v = [3,4]

     associate (c => v) ; call disp("1",v,c)
     c = c*10           ; call disp("2",v,c)
     v = [2,4,6]        ; call disp("3",v,c)
     c = c*10           ; call disp("4",v,c)
     v = [2]            ; call disp("5",v,c)
     end associate

     contains

     subroutine disp(label,v,c)
     character (len=*), intent(in) :: label
     integer, intent(in) :: v(:),c(:)
        write (*,"(a,' v = ',*(1x,i0))",advance="no") label,v
        write (*,"(3x,'c = ',*(1x,i0))") c
     end subroutine disp

     end program demonstrate_associate
```
### **Comparisons to other constructs**

When is it not true that
```fortran
   associate (a=>AA)
   end associate
```
is equivalent to
```fortran
   call assoc(AA)
   contains
   subroutine assoc(a)
   type(type(a)),intent(in) :: a(..) ! if a in an expression
   type(type(a))            :: a(..) ! if a in a variable
   end subroutine assoc
   ! somewhat like the parameters being class(*) but without all the
   ! SELECT statements like type(type(a)) worked.

   ! so "a" in the subroutine does not have the allocatable, optional,
   ! or pointer attributes even if AA did, and it is up to the programmer
   ! to make sure AA is allocated or assigned a target or present if
   ! optional when making the call if it has those attributes.

   ! but it can have the target attribute.
```
### **See Also**

  - [**do**(3)](#do) - construct
  - [**if**(3)](#if) - selects a block based on a sequence of logical
                       expressions.
  - [**cycle**(3)](#cycle) - construct
  - [**exit**(3)](#exit) - statement

  - [**associate**(3)](#associate) - associate construct
  - [**block**(3)](#block) - construct
  - [**goto**(3)](#goto) - jump to target line

  - [**select**(3)](#select) - select a block based on the value of an
                               expression (a case)
  - [**case**(3)](#case) - select a block based on the value of an
                           expression (a case)
  - [**endselect**(3)](#endselect) - select a block based on the value
                                     of an expression (a case)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
