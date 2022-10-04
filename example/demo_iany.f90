      program demo_iany
      use, intrinsic :: iso_fortran_env, only : integer_kinds, &
       & int8, int16, int32, int64
      implicit none
      logical,parameter :: T=.true., F=.false.
      integer(kind=int8) :: a(3), b(4:3)
         a(1) = int(b'00100100')
         a(2) = int(b'01101010')
         a(3) = int(b'10101010')
         write(*,*)'A='
         print '(1x,b8.8)', a
         write(*,*)'IANY(A)='
         print '(1x,b8.8)', iany(a)

         write(*,*)'IANY(A) with a mask'
         print '(1x,b8.8)', iany(a,mask=[T,F,T])
         write(*,*)'should match '
         print '(1x,b8.8)', iany([a(1),a(3)])
         write(*,*)'does it?'
         write(*,*)iany(a,[T,F,T]) == iany([a(1),a(3)])
      end program demo_iany
