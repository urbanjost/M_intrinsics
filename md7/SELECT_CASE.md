## select_case

### **Name**
   select_case(7f) - [FORTRAN:EXECUTION CONTROL] select a block based on the
   value of an expression (a case)
   
### **Synopsis**
The CASE construct selects for execution at most one of its constituent
blocks. The selection is based on the value of an expression.
```fortran
  [ case-construct-name : ] SELECT CASE (case-expr)
  CASE (value) [case-construct-name]
     [selected code]
  CASE ([lower_value]:[upper_value]) [case-construct-name]
     [selected code]
  CASE (range_or_value,range_or_value,...) [case-construct-name]
     [selected code]
  CASE DEFAULT
  END SELECT [ case-construct-name ]
```
  The expression may be _integer_,_character_,or _logical_. In particular
  it cannot be _real_.

  For a given case-construct, there shall be no possible
  value of the case-expr that matches more
  than one case-value-range.

  If the select-case-stmt of a case-construct specifies a
  case-construct-name, the corresponding end-select-stmt shall specify
  the same case-construct-name.

  If the select-case-stmt of a case-construct does not specify a
  case-construct-name, the corresponding end-select-stmt shall not
  specify a case-construct-name.

  If a case-stmt specifies a case-construct-name, the corresponding
  select-case-stmt shall specify the same case-construct-name.

  No more than one of the selectors of one of the CASE statements shall
  be DEFAULT.

  - For a given case-construct, each case-value shall be
    of the same type as case-expr 
  - For character type, the kind type parameters shall be the same
  - character length differences are allowed.
  - A case-value-range using a colon shall not be used if case-expr is
    of type logical.

### **Description**

   The execution of the SELECT CASE statement causes the case expression
   to be evaluated. The resulting value is called the case index. For a
   case value range list, a match occurs if the case index matches any
   of the case value ranges in the list. For a case index with a value
   of c, a match is determined as follows.

     1. If the case value range contains a single value v without a
        colon, a match occurs for type logical if the expression
        c .EQV. v is true, and a match occurs for type integer or
        character if the expression c == v is true.
     2. If the case value range is of the form low : high, a match
        occurs if the expression low <= c .AND. c <= high is true.
     3. If the case value range is of the form low :, a match occurs
        if the expression low <= c is true.
     4. If the case value range is of the form : high, a match occurs
        if the expression c <= high is true.
     5. If no other selector matches and a DEFAULT selector appears,
        it matches the case index.
     6. If no other selector matches and the DEFAULT selector does
        not appear, there is no match.

   The block following the CASE statement containing the matching
   selector, if any, is executed. This completes execution of the
   construct.

   It is permissible to branch to an end-select-stmt only from within
   its CASE construct.

### **Examples**

An integer signum function:
```fortran
integer function signum (n)
   select case (n)
   case (:-1)
      signum = -1  ! if <= -1 set to negative 1 
   case (0)
      signum = 0
   case (1:)
      signum = 1   ! anything >= 1 set to positive 1
   end select
end function signum
```
A code fragment to check for balanced parentheses:
```fortran
       character (80) :: line
          ...
       level = 0
       scan_line: do i = 1, 80
          check_parens: select case (line (i:i))
          case ('(')
             level = level + 1
          case (')')
             level = level - 1
             if (level < 0) then
                print *, 'unexpected right parenthesis'
                exit scan_line
             end if
          case default
             ! ignore all other characters
           end select check_parens
        end do scan_line
        if (level > 0) then
           print *, 'missing right parenthesis'
        end if
```

        the following three fragments are equivalent:

```fortran
        if (silly == 1) then
           call this
        else
           call that
        end if

        select case (silly == 1)
        case (.true.)
           call this
        case (.false.)
           call that
        end select

        select case (silly)
        case default
           call that
        case (1)
           call this
        end select
```
A code fragment showing several selections of one block:

```fortran
   select case (n)
      case (1, 3:5, 8)        ! selects 1, 3, 4, 5, 8
         call sub()
      case default
         call other()
   end select
```
