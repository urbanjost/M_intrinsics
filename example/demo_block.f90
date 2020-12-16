          program demo_block
          implicit none
          integer,parameter :: arr1(*)=[1,2,3,4,5,6,7]
          integer,parameter :: arr2(*)=[0,1,2,3,4,5,6,7]
            call showme(arr1)
            call showme(arr2)
            contains
          subroutine showme(a)
          integer,intent(in) :: a(:)
          integer :: i=-100
          integer :: tan
            tan=20 ! intentionally cause a conflict with intrinsic
            TESTFORZERO: block
               integer :: I  ! local block variable
               intrinsic :: tan  ! can use the TAN intrinsic in the block
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
             write(*,*)'this is the variable in the main scope of the program, I=',i
          end subroutine showme

          end program demo_block
