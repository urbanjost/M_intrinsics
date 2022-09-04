      program demo_atan2
      use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
      implicit none
      real(kind=sp) :: x = 1.e0_sp, y = 0.5e0_sp, z
         z = atan2(y,x)
         write(*,*)x,y,z
      end program demo_atan2
