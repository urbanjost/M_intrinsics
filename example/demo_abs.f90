      program demo_abs
      implicit none
      integer           :: i = -1
      real              :: x = -1.0
      complex           :: z = (-3.0,-4.0)
      doubleprecision   :: rr = -45.78d+00
      character(len=*),parameter :: &
       frmt =  '(1x,a15,1x," In: ",g0,            T51," Out: ",g0)', &
       frmtc = '(1x,a15,1x," In: (",g0,",",g0,")",T51," Out: ",g0)'
      integer,parameter :: dp=kind(0.0d0)

        ! any integer, real, or complex type
          write(*, frmt)  'integer         ',  i, abs(i)
          write(*, frmt)  'real            ',  x, abs(x)
          write(*, frmt)  'doubleprecision ', rr, abs(rr)
          write(*, frmtc) 'complex         ',  z, abs(z)

        ! any value whose positive value is representable
        ! A dusty corner is that abs(-huge(0)-1) of an integer would input
        ! a representable negative value on most machines but result in a
        ! positive value out of range.
          write(*, *) 'abs range test : ', abs(huge(0)), abs(-huge(0))
          write(*, *) 'abs range test : ', abs(huge(0.0)), abs(-huge(0.0))
          write(*, *) 'abs range test : ', abs(tiny(0.0)), abs(-tiny(0.0))

        ! elemental
          write(*, *) 'abs is elemental: ', abs([20,  0,  -1,  -3,  100])

        ! complex input produces real output
          write(*, *)  cmplx(30.0,40.0)

        ! the returned value for complex input can be thought of as the
        ! distance from the origin <0,0>
          write(*, *) 'distance of <XX,YY> from zero is', &
                     & distance(30.0_dp,40.0_dp)

          contains

          real(kind=dp) elemental function distance(x,y)
          real(kind=dp),intent(in) :: x,y
             ! dusty corners:
             ! note that KIND=DP is NOT optional
             ! if the desired result is KIND=dp.
             ! See cmplx(3).
             distance=abs( cmplx(x,y,kind=dp) )
          end function distance

      end program demo_abs
