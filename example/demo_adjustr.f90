      program demo_adjustr
      implicit none
      character(len=20) :: str = ' sample string '
         ! print a short number line
         write(*,'(a)')repeat('1234567890',5)

         !
         ! basic usage
         !
         str = adjustr(str)
         write(*,'(a)') str

         !
         ! elemental
         !
         write(*,'(a)')adjustr([character(len=50) :: &
         '  first           ', &
         '     second       ', &
         '         third    ' ])

         write(*,'(a)')repeat('1234567890',5)
      end program demo_adjustr
