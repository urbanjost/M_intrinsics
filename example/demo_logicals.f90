      program demo_different_logical_kinds
      use iso_fortran_env, only : logical_kinds
      use,intrinsic :: iso_fortran_env, only : &
       & LOGICAL8, LOGICAL16, LOGICAL32, LOGICAL64
      use,intrinsic :: iso_c_binding,   only : C_BOOL
      implicit none
      character(len=*),parameter             :: all='(*(g0))'
      ! potentially save space and improve performance by using the
      ! smallest available kind
      integer,parameter                      :: lk=selected_logical_kind(1)
      logical(lk)                            :: smallest_storage(10,20)

      ! C_BOOL is a kind compatible with C interfaces
      logical(kind=c_bool)                   :: boolean=.TRUE.

      integer                                :: i
        ! The integer array constant LOGICAL_KINDS() contains the kind
        ! values for supported logical kinds for the current processor
        print all, 'list LOGICAL kind values available on this platform'
         do i =1, size(logical_kinds)
            print all, '   integer,parameter :: boolean', &
            & logical_kinds(i),'=', logical_kinds(i)
         enddo

        print all, '   LOGICAL8  ==> KIND=',LOGICAL8
        print all, '   LOGICAL16 ==> KIND=',LOGICAL16
        print all, '   LOGICAL32 ==> KIND=',LOGICAL32
        print all, '   LOGICAL64 ==> KIND=',LOGICAL64
        print all, '   C_BOOL    ==> KIND=',C_BOOL

        print all, 'storage size of default logical = ', storage_size(.true.)
        print all, 'storage size of smallest logical kind = ', &
         storage_size(smallest_storage)
        print all, 'storage size of C_BOOL= ', storage_size(boolean)

        print all, 'kind of default logical = ', kind(.true.)
        print all, 'kind of smallest logical kind = ', kind(smallest_storage)
        print all, 'kind of C_BOOL= ', kind(.true._c_bool)

      end program demo_different_logical_kinds
  program demo_random_number use, intrinsic :: iso_fortran_env, only :
  dp=>real64 implicit none

  integer
    :: i, first, last, rand_int, sumup, passes real(kind=kind(0.0d0)) ::
    rand_val ! generate a lot of random integers from -10 to 100 and add to
    sum ! until upper limit is reached, for no reason first=-10 last=100
    sumup=0 passes=0 do while (sumup <= 1000000000) call
    random_number(rand_val) rand_int=first+floor((last+1-first)*rand_val)
    sumup=sumup+rand_int passes=passes+1 enddo
    write(*,*)'sumup=',sumup,'passes=',passes end program demo_random_number

ARRAY MASKING
  Logical arrays can be used as masks to selectively apply operations to
  elements of other arrays. This is particularly efficient for numerical
  computations.

          integer,parameter       :: isz=10
          real, dimension(isz)    :: a
          logical, dimension(isz) :: mask

          mask = (a > 5.0)
          ! Double elements of 'a' where 'a' is greater than 5.0
          a(mask) = a(mask) * 2.0

  A WHERE construct allows for multiple masks to be conditionally used.

          WHERE(cond1)
             ...
          ELSEWHERE(cond2)
             ...

   ELSEWHERE
   END WHERE
  Examples of masked array assignment are:

         WHERE (TEMP > 100.0) TEMP = TEMP - REDUCE_TEMP

         WHERE (PRESSURE <= 1.0)
            PRESSURE = PRESSURE + INC_PRESSURE
            TEMP = TEMP - 5.0

   ELSEWHERE
  RAINING = .TRUE.

   END WHERE
LOGICAL OPERATIONS
  Intrinsic operators like .AND., .OR., .NOT., and .EQV. (equivalent) or
  .NEQV. (not equivalent) are used to combine or negate logical expressions,
  creating more complex conditions.

          LOGICAL :: condition1, condition2, result

          condition1 = (value1 == 10)
          condition2 = (value2 /= 0)
          result = condition1 .OR. condition2

  [verify] is very powerful when using expressions as masks for processing
  strings. For example, to determine if strings represent valid Fortran symbol
  names:

      program fortran_symbol_name
      implicit none
      integer :: i
      ! some strings to inspect for being valid symbol names
      character(len=*),parameter :: symbols(*)=[character(len=10) :: &
       'A_ ', &
       '10 ', &
       'September ', &
       'A B', &
       '_A ', &
       ' ']

         write(*,'("|",*(g0,"|"))') symbols
         write(*,'("|",*(1x,l1,8x,"|"))') fortran_name(symbols)

      contains

      elemental function fortran_name(line) result (lout)
      ! determine if a string is a valid Fortran name
      ! ignoring trailing spaces (but not leading spaces)
      character(len=*),parameter   :: int='0123456789'
      character(len=*),parameter   :: lower='abcdefghijklmnopqrstuvwxyz'
      character(len=*),parameter   :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      character(len=*),parameter   :: allowed=upper//lower//int//'_'
      character(len=*),intent(in)  :: line
      character(len=:),allocatable :: name
      logical                      :: lout
         name=trim(line)
         if(len(name).ne.0)then
            ! first character is alphameric
            lout = verify(name(1:1), lower//upper) == 0  &
             ! verify other characters allowed in a symbol name
             & .and. verify(name,allowed) == 0           &
             ! check conforms to allowable length
             & .and. len(name) <= 63
         else
            lout = .false.
         endif
      end function fortran_name

      end program fortran_symbol_name

  Results:

       > |A_        |10        |September |A B       |_A        |          |
       > | T        | F        | T        | F        | F        | F        |

ARRAY REDUCTION FUNCTIONS
  Intrinsic functions like ALL() and ANY() are used to check if all or any
  elements in a logical array satisfy a condition, often used in conjunction
  with array masking.

          logical,parameter :: t=.true., f=.false.
          logical, dimension(5) :: status = [ t, f, t, t, t ]

          if (all(status)) then
             print *, "All statuses are true"
          endif

          if (any(status)) then
             print *, "At least one status is true"
          endif

BITWISE LOGICAL OPERATIONS
  For handling individual bits within integer variables, Fortran offers
  intrinsic functions like IAND (bitwise AND), IOR (bitwise OR), IEOR (bitwise
  exclusive OR), and NOT (bitwise NOT). These are crucial in low-level
  programming and certain numerical algorithms.

          integer :: a, b, c

          a = int(z'0101')
          b = int(z'0011')
          c = IAND(a, b) ! c will be 1 (0001)
          write(*,'*(g0,z0,1x)'),'a=',a,'b=',b,'c=',c

  but these return integer, not logical values and are mentioned only for
  reference.

CONDITIONAL EXPRESSIONS
  A conditional expression is related to logicals in that it is used to
  selectively evaluate a chosen subexpression.

      scalar-logical-expr ? expr [ : scalar-logical-expr ? expr ]... : expr )

  Each expr of a conditional-expr shall have the same declared type, kind type
  parameters, and rank.

  Examples of a conditional expression are:

         ( ABS(RESIDUAL)<=TOLERANCE ? "ok" : "did not converge" )
         ( I>0 .AND. I<=SIZE(A) ? A (I) : PRESENT(VAL) ? VAL : 0.0 )

  Conditional expressions are required to short-circuit (execute only the
  selected expression and not the other candidate) unlike the remainder of
  Fortran where short-circuiting behavior is typically left up to the
  processor.

  That is, elsewhere in Fortran it is not necessary for a processor to
  evaluate all of the operands of an expression, or to evaluate entirely each
  operand -- but the processor is free to evaluate all of the operands. That
  is, all of the operands may or may not be evaluated.

  This principle is most often applicable to logical expressions, zero-sized
  arrays, and zero-length strings, but it applies to all expressions.

  For example, in evaluating the expression

           X > Y .OR. L(Z)

  L(Z) may or may not be evaluated assuming "L" is a procedure name when the
  first condition (X > Y) is true.

LOGICALS CANNOT BE USED AS INTEGERS
  Logicals are not allowed in numeric expressions, as in common in several
  other languages. There is no automatic promotion of LOGICAL to INTEGER
  allowed by the standard or vice-versa. That being said, it is a common
  extension to cast .FALSE. to zero(0) and .TRUE. to some none-zero number;
  but what values are used and how many bits are significant in the values
  varies widely between current popular compilers and so the extension should
  be avoided.

  Sample program:

      program logical_integer
      implicit none
      character(len=*),parameter            :: all='(*(g0))'
      integer                               :: i1, i2
      ! make T and F abbreviations for .TRUE. and .FALSE.
      logical,parameter                     :: T=.true., F=.false.
      logical                               :: l1, l2

        print all, 'MERGE() is one method for transposing logical and integer'
        ! converting a logical to an integer is not done
        ! with LOGICAL(3) and INT(3) or promotion by assignment;
        ! but can be done with MERGE(3) with scalars or arrays.
         i1=merge(1,0,T)
         i2=merge(1,0,F)
         write(*,all)'   T-->',i1,' F-->',I2
         l1=merge(T,F,i1.eq.0)
         l2=merge(T,F,i2.eq.0)
         write(*,all)'   0-->',l1,' 1-->',l2
      end program logical_integer

  Results:

       > MERGE() is one method for transposing logical and integer
       >    T-->1 F-->0
       >    0-->F 1-->T

LOGICAL EDITING
  The Lw edit descriptor indicates that the field occupies w positions.  The
  input field so specified consists of optional blanks, optionally followed by
  a period, followed by a "T" for true or "F" for false. The "T" or "F" may be
  followed by additional characters in the field, which are ignored.

  So, for example the strings ".TRUE." and ".FALSE." are acceptable input
  forms if "w" is sufficiently sized.

  A lower-case letter is equivalent to the corresponding upper-case letter in
  a logical input field.

  The output ﬁeld consists of w−1 blanks followed by a T or F, depending on
  whether the internal value is true or false, respectively.

      program logical_formatted
      implicit none
      character(len=*),parameter    :: all='(*(g0))'
      character(len=:),allocatable  :: line
      logical                       :: array(8), p, q
        print all, 'Logicals print as the right-justified string "T" or "F"'
        write(*,'("[",l10,"]")') .TRUE.
        write(*,'("[",l0,"]")')  .FALSE.
        print all, 'the first non-blank letter after an optional period'
        print all, 'determines the value on input'
        print all, repeat('1234567',8)
        line='.false. .true.  T    F       TrustyFake!!!tr     fffffff'
        print all, line
        read(line,'(8(L7))') array
        print all, array
      end program logical_formatted

  Results:

       > Logicals print as the right-justified string "T" or "F"
       > [         T]
       > [F]
       > the first non-blank letter after an optional period
       > determines the value on input
       > 12345671234567123456712345671234567123456712345671234567
       > .false. .true.  T    F       TrustyFake!!!tr     fffffff
       > FTTFTFTF

  The G edit descriptor also may be used to edit logical data.

SEE ALSO
  Bit-level procedures

  •  ieor(3), ior(3), ishftc(3), ishft(3), iand(3).

  •  result = iall(array [,mask]) | iall(array ,dim [,mask])

  •  result = iany(array [,mask]) | iany(array ,dim [,mask])

  •  result = iparity( array [,mask] ) | iparity( array, dim [,mask] )

  •  result = maskl( i [,kind] )

  •  result = maskr( i [,kind] )

  •  result = merge_bits(i, j, mask) ! Merge bits using a mask

  Other

  •  VERIFY(3) is very powerful when using expressions as masks for processing
     strings

  •  [[iso_fortran_env]] module

  •  iso_c_binding module

  •  TRANSFER(3) - Transfer bit patterns

  Fortran Tutorials(license: MIT) @urbanjost

                               January 18, 2026             logicals(7fortran)
