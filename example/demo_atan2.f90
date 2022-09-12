      program demo_atan2
      real :: x, y, z
      complex :: c

       ! basic usage
        ! ATAN2 (1.5574077, 1.0) has the value 1.0 (approximately).
        z=atan2(1.5574077, 1.0)
        write(*,*) 'radians=',z,'degrees=',r2d(z)

       ! elemental arrays
        write(*,*)'elemental',atan2( [10.0, 20.0], [30.0,40.0] )
       ! elemental arrays and scalars
        write(*,*)'elemental',atan2( [10.0, 20.0], 50.0 )

       ! break into real and imaginary components to use with complex values
       ! note TAN2() can take a complex value
        c=(0.0,1.0)
        write(*,*)'complex',c,atan2( x=c%re, y=c%im )
        COMPLEX_VALS: block
        real                :: ang, radius
        complex,allocatable :: vals(:)

        vals=[ &
          ( 0.0, 1.0 ), &
          ( 1.0, 1.0 ), &
          ( 1.0, 0.0 ), &
          ( 0.0,-1.0 ), &
          (-1.0, 1.0 ), &
          (-1.0, 0.0 ), &
          (-1.0,-1.0 )]
        do i=1,size(vals)
           call cartesian_to_polar(vals(i)%re,vals(i)%im,radius,ang)
           write(*,101)vals(i),ang,r2d(ang),radius
        enddo
        101 format('X= ',f5.2,' Y= ',f5.2,' ANGLE= ',g0,T40,'DEGREES= ',g0.4,T57,'DISTANCE=',g0)
       endblock COMPLEX_VALS

      contains

      elemental real function r2d(radians)
      ! input radians to convert to degrees
      doubleprecision,parameter :: DEGREE=0.017453292519943d0 ! radians
      real,intent(in)           :: radians
         r2d=radians / DEGREE ! do the conversion
      end function r2d

      subroutine cartesian_to_polar(x,y,radius,inclination)
      implicit none
      real,intent(in)  :: x,y
      real,intent(out) :: radius,inclination
         radius=sqrt(x**2+y**2)
         if(radius.eq.0)then
            inclination=0.0
         else
            inclination=atan2(y,x)
         endif
      end subroutine cartesian_to_polar

      end program demo_atan2
