      program demo_reshape
      implicit none
      integer :: i
      integer, dimension(4) :: x=[(i,i=10,40,10)]
      real :: xx(3,4)
      real,allocatable :: v(:)
          ! x is originally a vector with four elements
          write(*,*) shape(x) ! what is the current shape of the array?
          write(*,*) shape(reshape(x, [2, 2]))    ! prints "2 2"

          ! pack any array into a vector
          xx=1.0
          v=reshape(xx,[size(xx)])
          write(*,*)shape(v),ubound(v)
      end program demo_reshape
