      program demo_present
      implicit none
         write(*,*) func(), func(42)
      contains

      integer function func(x)
      integer, intent(in), optional :: x
         if(present(x))then
           func=x**2
         else
           func=0
         endif
      end function

      end program demo_present
