      program demo_aint
      use, intrinsic :: iso_fortran_env, only : sp=>real32, dp=>real64
      implicit none
      real(kind=sp) :: x4
      real(kind=dp) :: x8

         x4 = 4.3210_sp
         x8 = 4.3210_dp
         print *, aint(x4), aint(x8)
         print *
         ! elemental
         print *,aint([ &
          &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
          &  0.0,   &
          &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

      end program demo_aint
