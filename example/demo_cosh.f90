      program demo_cosh
      use, intrinsic :: iso_fortran_env, only : real32, real64, real128
      implicit none
      real(kind=real64) :: x = 1.0_real64
          write(*,*)'X=',x,'COSH(X=)',cosh(x)
      end program demo_cosh
