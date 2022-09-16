      program demo_dprod
      use, intrinsic :: iso_fortran_env, only : real_kinds, &
      & real32, real64, real128
      implicit none
      integer,parameter :: dp=kind(0.0d0)
      real :: x = 5.2
      real :: y = 2.3
      real(kind=dp) :: dd

         ! basic usage
         dd = dprod(x,y)
         print *, 'compare dprod(xy)=',dd, &
         & 'to x*y=',x*y, &
         & 'to dble(x)*dble(y)=',dble(x)*dble(y)

         ! elemental
         print *, dprod( [2.3,3.4,4.5], 10.0 )
         print *, dprod( [2.3,3.4,4.5], [9.8,7.6,5.4] )

         ! other interesting comparisons
         print *, 'integer multiplication of digits=',52*23
         print *, 52*23/100.0
         print *, 52*23/100.0d0

         !! A common extension is to take doubleprecision arguments
         !! and return higher precision when available
         bigger: block
         doubleprecision :: xx = 5.2_dp
         doubleprecision :: yy = 2.3_dp
         print *, 'dprop==>',dprod(xx,yy)
         print *, 'multipy==>',xx*yy
         print *, 'using dble==>',dble(xx)*dble(yy)
         print *, 'kind of arguments is',kind(xx)
         print *, 'kind of result is',kind(dprod(xx,yy))
         endblock bigger

      end program demo_dprod
