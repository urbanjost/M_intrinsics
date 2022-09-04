      program demo_allocated
      use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
      implicit none
      integer :: i = 4
      real(kind=sp), allocatable :: x(:)

         ! if already allocated, deallocate
         if ( allocated(x) ) deallocate(x)

         ! only if not allocated, allocate
         if ( .not. allocated(x) ) allocate(x(i))

         write(*,*)allocated(x), size(x)
         if( allocated(x)) then
             write(*,*)'do things if allocated'
         else
             write(*,*)'do things if not allocated'
         endif
         call intentout(x)
         write(*,*)'note it is deallocated!',allocated(x)
         contains
         subroutine intentout(arr)
         ! note that if arr has intent(out) and is allocatable,
         ! arr is deallocated on entry
         real(kind=sp),intent(out),allocatable :: arr(:)
             write(*,*)'note it was allocated in calling program',allocated(arr)
         end subroutine intentout

    end program demo_allocated
