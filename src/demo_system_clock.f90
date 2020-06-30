             program demo_system_clock
               integer :: count, count_rate, count_max
               call system_clock(count, count_rate, count_max)
               write(*,*) count, count_rate, count_max
             end program demo_system_clock
