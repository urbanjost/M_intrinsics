      program demo_aint
      use, intrinsic :: iso_fortran_env, only : real32, real64
      implicit none
      real(kind=real32) :: x4
      real(kind=real64) :: x8

         x4 = 4.3210_real32
         x8 = 4.3210_real64
         print *, aint(x4), aint(x8)
         print *
         ! elemental
         print *,aint([ &
          &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
          &  0.0,   &
          &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

      end program demo_aint
