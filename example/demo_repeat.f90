      program demo_repeat
      implicit none
      integer :: i, j
          write(*,'(a)') repeat("^v", 36)         ! line break
          write(*,'(a)') repeat("_", 72)          ! line break
          write(*,'(a)') repeat("1234567890", 7)  ! number line
          do i=80,0,-1 ! a simple progress bar
              write(*,'(a)',advance='no') &
              & repeat("#", i)//repeat(' ',80-i)//char(13)
              !do something slow
          enddo
      end program demo_repeat
