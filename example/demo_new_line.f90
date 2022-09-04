      program demo_new_line
      implicit none
      character,parameter :: nl=new_line('a')
      character(len=:),allocatable :: string

         string='This is record 1.'//nl//'This is record 2.'
         write(*,'(a)') string

         write(*,'(*(a))',advance='no') &
            nl,'This is record 1.',nl,'This is record 2.',nl

    end program demo_new_line
