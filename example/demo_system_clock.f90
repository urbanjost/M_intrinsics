      program demo_system_clock
      implicit none
      integer, parameter :: wp = kind(1.0d0)
      integer :: count, count_rate, count_max
      integer :: start, finish
      real    :: time_read

         call system_clock(count, count_rate, count_max)
         write(*,*)'COUNT_MAX=',count_max
         write(*,*)'COUNT_RATE=',count_rate
         write(*,*)'CURRENT COUNT=',count

         call system_clock(start)
         ! <<<< code to time
         call system_clock(finish)

         time_read=(finish-start)/real(count_rate,wp)
         write(*,'(a30,1x,f7.4,1x,a)') 'time * : ', time_read, ' seconds'

      end program demo_system_clock
