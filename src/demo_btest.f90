           program demo_btest
               integer :: i = 32768 + 1024 + 64
               integer :: pos
               logical :: bool
               do pos=0,16
                   bool = btest(i, pos)
                   print *, pos, bool
               end do
           end program demo_btest
