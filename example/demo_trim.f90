      program demo_trim
      implicit none
      character(len=10), parameter :: s = "gfortran  "
         write(*,*) len(s), len(trim(s))  ! "10 8", with/without trailing blanks

         ! with/without trailing blanks
         write(*,*) len(s), len(trim('   leading'))
         write(*,*) len(s), len(trim('   trailing    '))
         write(*,*) len(s), len(trim('               '))

      end program demo_trim
