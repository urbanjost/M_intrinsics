      program demo_sqrt
      use, intrinsic :: iso_fortran_env, only : real_kinds, &
       & real32, real64, real128
      implicit none
      real(kind=real64) :: x, x2
      complex :: z, z2

         x = 2.0_real64
         z = (1.0, 2.0)
         write(*,*)x,z

         x2 = sqrt(x)
         z2 = sqrt(z)
         write(*,*)x2,z2

         x2 = x**0.5
         z2 = z**0.5
         write(*,*)x2,z2

      end program demo_sqrt
