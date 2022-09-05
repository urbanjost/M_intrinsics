      program demo_maskl
      implicit none
      integer :: i
         i=maskl(1)
         write(*,'(i0,1x,b0,/)') i,i
         ! elemental
         write(*,'(*(i11,1x,b0,1x,/))') maskl([(i,i,i=1,bit_size(0))])
      end program demo_maskl
