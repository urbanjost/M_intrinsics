## associate

### **Name**
   ASSOCIATE(7f) - [EXECUTION CONTROL] aliases complex variable names
   and creates parameters from expressions

### **Syntax**
Syntax:

  [ construct-name : ] ASSOCIATE ( associate-name => selector ...)
    :
    : a sequence of zero or more statements or constructs.
    :
  END ASSOCIATE [ construct-name ]

   ASSOCIATE-NAME

   An identifier that becomes associated with the selector in the
   ASSOCIATE block. The "associate-name" must be unique within the
   construct.

   SELECTOR

   Is an expression or variable. It becomes the associated entity.

   CONSTRUCT-NAME

   If a construct-name is specified the sme name must appear at both
   the beginning of an ASSOCIATE and at its associated END ASSOCIATE
   statement.

   The same construct-name must not be used for different named
   constructs in the same scoping unit.

   It is permissible to branch to an end-associate-stmt only from
   within its ASSOCIATE construct.
```fortran
        exit ASSOCIATE
```
### **Description**

An ASSOCIATE statement creates either an

1. abbreviation for the value of an expression that becomes a
   constant in the ASSOCIATE block.

   or

2. an alias for a long variable name. If the LHS is a variable it
   can actually have the value change the associated name outside
   of the block as well.

An alias for a variable is useful when you want to simplify multiple
accesses to a variable that has a lengthy description; for example,
if the variable contains multiple subscripts and component names.

A constant is similiar to a PARAMETER statement where the value can be
changed at runtime before entering the ASSOCIATE block.

It is _NOT_ equivalent to a function macro.

The ASSOCIATE construct associates named entities with expressions or
variables during the execution of its block. These "named construct
entities" are "associating entities". The names are
"associate names".

  o If selector is not a variable or is a variable that
    has a vector subscript, associate-name shall not appear in
    a variable definition context.

  o An associate-name shall not be the same as another
    associate-name in the same associate-stmt.

  o variable shall not be a coindexed object.

  o expr shall not be a variable.

  o If present, an associate-construct-name must appear on both the
    ASSOCIATE statement and the ENDASSOCIATE statement.

### **Execution of the Associate Construct**

  Execution of an ASSOCIATE construct causes evaluation of every
  expression within every selector that is a variable designator and
  evaluation of every other selector , followed by execution of its
  block.

  During execution of the block within the construct, each associate-name
  identifies an entity, which is associated with the corresponding
  selector.

  The associating entity assumes the declared type and type parameters
  of the selector.

  If and only if the selector is polymorphic, the associating entity
  is polymorphic.

### **Attributes of Associate Names**

  Within an ASSOCIATE or SELECT TYPE construct, each associating entity
  has the same rank and corank as its associated selector.

  The lower bound of each dimension is the result of the intrinsic
  function LBOUND(3f) applied to the corresponding dimension of selector.
  The upper bound of each dimension is one less than the sum of the
  lower bound and the extent.

  The cobounds of each codimension of the associating entity are the same
  as those of the selector. The associating entity has the ASYNCHRONOUS
  or VOLATILE attribute if and only if the selector is a variable and has
  the attribute. The associating entity has the TARGET attribute if and
  only if the selector is a variable and has either the TARGET or POINTER
  attribute.

  If the associating entity is polymorphic, it assumes the dynamic type
  and type parameter values of the selector. If the selector has the
  OPTIONAL attribute, it shall be present.

  The associating entity is contiguous if and only if the selector
  is contiguous.

  If the selector is not permitted to appear in a variable definition
  context, the associate name shall not appear in a variable
  definition context.

### **Examples**
  The following shows an expression as a selector:
```fortran
   ASSOCIATE (O => (A-F)**2 + (B+G)**2)
     PRINT *, SQRT (O)
   END ASSOCIATE
```
  The following shows association with an array section:
```fortran
   ASSOCIATE (ARRAY => AB % D (I, :) % X)
     ARRAY (3) = ARRAY (1) + ARRAY (2)
   END ASSOCIATE
```
  Without the ASSOCIATE construct, this is what you would need to write:
```fortran
   AB % D (I, 3) % X = AB % D (I, 1) % X + AB % D (I, 2) % X
```
  The following example illustrates an association with an expression.
```fortran
       associate ( z => exp(-(x**2+y**2)) * cos(theta) )
       ! creates the constant "z"
         print *, a+z, a-z
       end associate
```
  The following example illustrates an association with a
  derived-type variable.
```fortran
       associate ( xc => ax%b(i,j)%c )
         xc%dv = xc%dv + product(xc%ev(1:n))
       end associate
```
  The following example illustrates association with an array section.
```fortran
       associate ( array => ax%b(i,:)%c )
         array(n)%ev = array(n-1)%ev
       end associate
```
  The following example illustrates multiple associations.
```fortran
       associate ( w => result(i,j)%w, zx=>ax%b(i,j)%d, zy=>ay%b(i,j)%d )
         w = zx*x + zy*y
       end associate
```
   The following example uses the ASSOCIATE construct as a shorthand for a
   complex expression and renames an existing variable, MYREAL. After the
   end of the ASSOCIATE construct, any change within the construct to the
   value of the associating entity that associates with MYREAL is reflected.
```fortran
      program demo_associate
      real :: myreal, x, y, theta, a
      x = 0.42
      y = 0.35
      myreal = 9.1
      theta = 1.5
      a = 0.4
      associate ( z => exp(-(x**2+y**2)) * cos(theta), v => myreal)
         print *, a+z, a-z, v
         v = v * 4.6
      end associate
      print *, myreal
      end program demo_associate
```
  See if you know what the following produces ...
```fortran
      program dustycorner
      implicit none
      real :: a = 42
      associate (a => a, b => 2 * a)
         print *, a, b
         a = 0
         print *, a, b
      end associate
      print *, a

      a=42
      associate (aa => a, b => 2 * a)
         print *, a, aa, b
         aa = 0
         print *, a, aa, b
         a=-1
         print *, a, aa, b
      end associate
      print *, a
      end program dustycorner
```
  Did you expect the following?
```text
      42.0000000       84.0000000
      0.00000000       84.0000000
      0.00000000
      42.0000000       42.0000000       84.0000000
      0.00000000       0.00000000       84.0000000
     -1.00000000      -1.00000000       84.0000000
     -1.00000000
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
