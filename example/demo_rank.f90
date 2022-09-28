      program demo_rank
      implicit none
      integer :: a
      real, allocatable :: b(:,:)
      real  :: c(10,20,30)
      complex :: d
      ! make up a type
      type mytype
         integer :: int
         real :: float
         character :: char
      end type mytype
      type(mytype) :: any_thing(1,2,3,4,5)

         print *, 'rank of scalar a=',rank(a)
         ! note you can query this array even though not allocated
         print *, 'rank of matrix b=',rank(b)
         print *, 'rank of vector c=',rank(c)
         print *, 'rank of scalar d=',rank(d)
         ! you can query any type
         print *, 'rank of any_thing=',rank(any_thing)

         call query_int(10)
         call query_int([20,30])
         call query_int( reshape([40,50,60,70],[2,2]) )

      contains

      subroutine query_int(entity)
      ! It is hard to do much with something dimensioned
      ! name(..) if not calling C but one thing you can
      ! do is call the inquiry functions ...
      integer,intent(in) :: entity(..)

         if(rank(entity).eq.0)then
            write(*,*)'you passed a scalar',rank(entity)
         else
            write(*,*)'you passed an array, rank=',rank(entity)
         endif

      end subroutine query_int

      end program demo_rank
