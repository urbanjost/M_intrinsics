## enumeration

### **Description**

#### Enumeration types
An enumeration type is a nonintrinsic type with no type parameter. It
is not a derived type and is not inter- operable. An enumeration type
deﬁnition deﬁnes the name of the type and lists all the possible
values of the type.
```text
R766  enumeration-type-def     is enumeration-type-stmt
                                      enumeration-enumerator-stmt
                                      [ enumeration-enumerator-stmt ]...
                                      end-enumeration-type-stmt
R767  enumeration-type-stmt    is ENUMERATIONTYPE[[,access-spec ] :: ]
enumeration-type-name
```
C7114 An access-spec on an enumeration-type-stmt shall only appear in
the speciﬁcation part of a module.

R768   enumeration-enumerator-stmt is   ENUMERATOR[:: ]
enumerator-name-list

R769   end-enumeration-type-stmt  is
ENDENUMERATIONTYPE[enumeration-type-name ]

C7115 If enumeration-type-name appears on an END ENUMERATION TYPE
statement, it shall be the same as on the ENUMERATION TYPE statement.

The access-spec on an ENUMERATION TYPE statement speciﬁes
the accessibility of the enumeration-type- name and the default
accessibility of its enumerators. The accessibility of an enumerator
may be conﬁrmed or overridden by an access-stmt.

Each enumerator in the defnition is a scalar named constant of the
enumeration type. The order of thee numerator names in the definition
defines the ordinal position of each enumerator.

R770   enumeration-type-spec      is  enumeration-type-name

C7116 The enumeration-type-name in an enumeration-type-spec shall be
the name of a previously deﬁned enumeration type.

An enumeration type speciﬁer speciﬁers the type. Two data entities of
enumeration type have the same type if they are declared with reference
to the same enumeration type deﬁnition.

R771   enumeration-constructor    is  enumeration-type-spec (
scalar-int-expr )

An enumeration constructor produces the scalar value of the enumeration
type whose ordinal position is the value of the scalar-int-expr. The
scalar-int-expr shall have a value that is positive and less than
or equal to the number of enumerators in the enumeration type’s
deﬁnition.

#### NOTE
Here is an example of a module deﬁning two enumeration types.
```fortran
Module enumeration_mod
   Enumeration Type :: v_value
   Enumerator :: v_one, v_two, v_three
   Enumerator v_four
   End Enumeration Type
   Enumeration Type :: w_value
   Enumerator :: w1, w2, w3, w4, w5, wendsentinel
   End Enumeration Type
Contains
   Subroutine sub(a)
      Type(v_value),Intent(In) :: a
      Print 1,a ! Acts similarly to Print *,Int(a).
1     Format(’A has ordinal value ’,I0)
   End Subroutine
   Subroutine wcheck(w)
      Type(w_value),Intent(In) :: w
      Select Case(w)
       Case(w1)
         Print *,’w1 selected’
       Case (w2:w4)
         Print *,’One of w2...w4 selected’
       Case (wendsentinel)
         Stop ’Invalid w selected’
       Case Default
         Stop ’Unrecognized w selected’
      End Select
   End Subroutine
End Module
Here is an example of a program using that module.
Program example
   Use enumeration_mod
   Type(v_value) :: x=v_one
   Type(v_value) :: y=v_value(2) ! Explicit constructor producing v_two.
   Type(v_value) :: z,nz         ! Initially undefined.
   Call sub(x)
   Call sub(v_three)
   z = v_value(1)                ! First value.
   Do
      If (z==Huge(x)) Write (*,’(A)’,Advance=’No’) ’ Huge:’
      Call sub(z)
      nz = Next(z)
      If (z==nz) Exit
      z = nz
   End Do
End Program
```
Here is an example showing some invalid usages of enumerations.
```fortran
Program invalid
   Use enumeration_mod
   Type(v_value) :: a, b
   a = 1         ! INVALID - wrong type (INTEGER).
   b = w1        ! INVALID - wrong enumeration type.
   Print *,a     ! INVALID - list-directed i/o not available.
End Program
```
An enumeration type can be used to declare components, for example:
```fortran
Module example2
   Use enumeration_mod
   Type vw
      Type(v_value) v
      Type(w_value) w
   End Type
Contains
   Subroutine showme(ka)
      Type(vw),Intent(In) :: ka
      Print 1,ka
1     Format(1X,’v ordinal is ’,I0,’, w ordinal is ’,I0)
   End Subroutine
End Module
```
