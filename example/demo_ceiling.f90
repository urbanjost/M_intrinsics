      program demo_ceiling
      implicit none
      real :: x = 63.29
      real :: y = -63.59
         print *, ceiling(x)
         print *, ceiling(y)
         ! elemental
         print *,ceiling([ &
         &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
         &  0.0,   &
         &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])
      end program demo_ceiling
