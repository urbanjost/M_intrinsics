           program demo_asin
           use, intrinsic :: iso_fortran_env, only : real_kinds, real32, real64, real128
             real(kind=real64) :: x = 0.866_real64
             x = asin(x)
           end program demo_asin
