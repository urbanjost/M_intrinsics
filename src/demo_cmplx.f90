           program demo_cmplx
               integer :: i = 42
               real :: x = 3.14
               complex :: z
               z = cmplx(i, x)
               print *, z, cmplx(x)
           end program demo_cmplx
