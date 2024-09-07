  program demo_associate implicit none character(len=*),parameter ::
  g='(*(g0,1x))'

  character :: array(-5:5,-5:5)
    ! custom non-normal bounds ! note the different between queries of ARRAY
    versus ARRAY(:,:)

    write(*,g)'array:
      write(*,g)'array(:,:): ', 'lbound=',lbound(array(:,:)), & ! the bounds
      assigned to the identifiers are what UBOUND(3f) ! and LBOUND(3f) return
      given the selector as an argument associate ( &

      alias=>
        array,              & ! keeps the custom bounds

      normal=>
        array(:,:),         & ! gets normal bounds

      quadI=>
        array(+1:+5,-5:-1), & ! quad* will have normal bounds

      quadII=>
        array(-5:-1,-5:-1), & !  quadIII=> array(-5:-1,+1:+5), & !

      quadIV=>
        array(+1:+5,+1:+5), & !  xaxis=>array(:,0), & yaxis=>array(0,:) & )
        array='.' ! selector name is still valid in the block xaxis='-'
        yaxis='|' alias(0,0)='+' ! uses non-normal bounds, equivalent to
        array(0,0)='+' write(*,'(11(g0,1x))') alias ! the quads have
        normalized dimension bounds (1:5,1:5):

      quadI
        =  '1';  quadI(1,1)    =  'a';  quadI(5,5)    =  'A'

      quadII
        =  '2';  quadII(1,1)   =  'b';  quadII(5,5)   =  'B'

      quadIII
        =  '3';  quadIII(1,1)  =  'c';  quadIII(5,5)  =  'C'

      quadIV
        =  '4';  quadIV(1,1)   =  'd';  quadIV(5,5)   =  'D'
        write(*,'(11(g0,1x))') alias

      write(*,g)'array:
        lbound=',lbound(array),  'ubound=',ubound(array)

      write(*,g)'alias:
        lbound=',lbound(alias),  'ubound=',ubound(alias)

      write(*,g)'normal:
        lbound=',lbound(normal), 'ubound=',ubound(normal)

      write(*,g)'quadI:
        lbound=',lbound(quadI),  'ubound=',ubound(quadI)

      write(*,g)'quadII:
        lbound=',lbound(quadII), 'ubound=',ubound(quadII)

      write(*,g)'quadIV:
        lbound=',lbound(quadIV), 'ubound=',ubound(quadIV) end associate end
        program demo_associate

  Results:

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

  Dusty Corners

    If the expressions have side-effects are they executed only when the block
    is entered?

    Selected variable names are still accessible in the ASSOCIATE block.  This
    is confusing and should be avoided, particular if the selectors are
    allocatable or pointers. This is similiar to variables passed as arguments
    to contained procedures but referenced via the argument name and the name
    in the surrounding scope. The behavior is ill-defined. Does a change to
    the argument take affect immediately or upon return from the procedure? If
    the argument is not declared allocatable or is a pointer does the argument
    name value get changed by deallocation or disassociation or changes to the
    original names?

    are you allowed to allocate v to a different size before the ASSOCIATE is
    terminated? If so, what happens to c ?

    Does that mean it is invalid to resize v within the ASSOCIATE block? Or is
    it only invalid to resize v and then refer to c? Or only invalid to resize
    v and refer to c when c is associated with elements of v that no longer
    exist?

           implicit none
           integer, allocatable, target :: v(:)
           integer, pointer :: p(:)
              v = [4,7,9]
              p => v
              print*,p
              deallocate(v)
              print*,p ! invalid, because target has been deallocated
           end program main

    are you allowed to allocate v to a different size before the ASSOCIATE is
    terminated? If so, what happens to c? ```fortran program
    demonstrate_associate implicit none integer, allocatable :: v(:) v = [3,4]

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

COMPARISONS TO OTHER CONSTRUCTS
  When is it not true that

         associate (a=>AA)
         end associate

  is equivalent to

         call assoc(AA)
         contains
         subroutine assoc(a)
         type(type(a)),intent(in) :: a(..) ! if a in an expression
         type(type(a))            :: a(..) ! if a in a variable
         end subroutine assoc

         ! somewhat like the parameters being class(*) but without all the SELECT statements
         ! like type(type(a)) worked.

         ! so "a" in the subroutine does not have the allocatable, optional, or pointer
         ! attributes even if AA did, and it is up to the programmer to make sure AA is allocated
         ! or assigned a target or present if optional when making the call if it has those
         ! attributes.

         ! but it can have the target attribute.

SEE ALSO
  •  DO(3) - construct

  •  IF(3) - selects a block based on a sequence of logical expressions.

  •  CYCLE(3) - construct

  •  EXIT(3) - statement

  •  ASSOCIATE(3) - associate construct

  •  BLOCK(3) - construct

  •  GOTO(3) - jump to target line

  •  SELECT(3) - select a block based on the value of an expression (a case)

  •  CASE(3) - select a block based on the value of an expression (a case)

  •  ENDSELECT(3) - select a block based on the value of an expression (a
     case)

  Fortran intrinsic descriptions (license: MIT) @urbanjost

                              September 07, 2024           associate(7fortran)
