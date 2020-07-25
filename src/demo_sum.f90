           program demo_sum
           implicit none
             integer :: x(5) = [ 1, 2, 3, 4 ,5 ]
             print *, sum(x)                        ! all elements, sum = 15
             print *, sum(x, mask=mod(x, 2)==1)     ! odd elements, sum = 9
           end program demo_sum
