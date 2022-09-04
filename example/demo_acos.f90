      program demo_acos
      use, intrinsic :: iso_fortran_env, only : real_kinds,real32,real64,real128
      implicit none
      character(len=*),parameter :: all='(*(g0,1x))'
      real(kind=real64) :: x = 0.866_real64
      real(kind=real64),parameter :: d2r=acos(-1.0_real64)/180.0_real64

          print all,'acos(',x,') is ', acos(x)
          print all,'90 degrees is ', d2r*90.0_real64, ' radians'
          print all,'180 degrees is ', d2r*180.0_real64, ' radians'
          print all,'for reference &
          &PI ~ 3.14159265358979323846264338327950288419716939937510'
          print all,'elemental',acos([-1.0,-0.5,0.0,0.50,1.0])

    end program demo_acos
