           program demo_char
               integer :: i = 74
               character(1) :: c
               c = char(i)
               print *, i, c ! returns 'J'
           end program demo_char
