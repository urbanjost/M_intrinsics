           program demo_adjustl
             character(len=20) :: str = '   gfortran'
             str = adjustl(str)
             print *, str
           end program demo_adjustl
