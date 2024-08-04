## return

### **Name**
   return(7f) - [STATEMENT] completes execution of the instance
   of the subprogram in which it appears

### **Synopsis**
```fortran
   RETURN [scalar-int-expr]
```
### **Description**
   Execution of the RETURN statement completes execution of the instance
   of the subprogram in which it appears.

   It is generally considered good practice to avoid having multiple
   RETURN statements in a single subprogram. A RETURN is not required
   in a subprogram as reaching the end of the subprogram is equivalent
   to execution of a RETURN statement with no expression.

   The RETURN statement must appear in the scoping unit of a function or
   subroutine subprogram.

### **Options**
   scalar-int-expr  Alternate returns are deprecated!

                    If the expression appears and has a value n between
                    1 and the number of asterisks in the dummy argument
                    list, the CALL statement that invoked the subroutine
                    transfers control to the statement identified by
                    the nth alternate return specifier in the actual
                    argument list of the referenced procedure. If the
                    expression is omitted or has a value outside the
                    required range, there is no transfer of control to
                    an alternate return.

                    The scalar-int-expr is allowed only in the scoping
                    unit of a subroutine subprogram.
### **Example**
  Sample program
```fortran
   program demo_return
      call tryreturn(1)
      call tryreturn(10)
   contains
      subroutine tryreturn(i)
         integer,intent(in) :: i
         select case(i)
          case(1)
            write(*,*)'*one*'
            return
          case(2)
            write(*,*)'*two*'
            return
          case default
            write(*,*)'*default*'
            return
         end select
         write(*,*)'*cannot get here*'
         return
      end subroutine tryreturn
   end program demo_return
```
Results:
```text

 >  *one*
 >  *default*
```
  Sample program using alternate returns. Alternate returns are
  an obsolescent feature.
```fortran
   program alt_return
   implicit none
      call one(2,*10,*20,*30)
      write(*,*)'did not select alternate return'
      goto 999
   10 continue
      write(*,*)'picked first alternate return'
      goto 999
   20 continue
      write(*,*)'picked second alternate return'
      goto 999
   30 continue
      write(*,*)'picked third alternate return'
      goto 999
   999 continue
   contains
   subroutine one(ipick,*,*,*)
   implicit none
   integer :: ipick
      select case(ipick)
       case(1)
         write(*,*)'first alternate return selected'
         return 1
       case(2)
         write(*,*)'second alternate return selected'
         return 2
       case(3)
         write(*,*)'third alternate return selected'
         return 3
      end select
      write(*,*)'no alternate return selected'
   end subroutine one
   end program alt_return
```
Results:
```text
 >  second alternate return selected
 >  picked second alternate return
```
 _fortran-lang statement descriptions (license: MIT) \@urbanjost_
