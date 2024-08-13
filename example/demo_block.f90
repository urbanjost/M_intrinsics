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
                             ! as this definition supercedes the one in the
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
