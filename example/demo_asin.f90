    program demo_asin use, intrinsic :: iso_fortran_env, only : dp=>real64 implicit none ! value to convert degrees to radians
    real(kind=dp),parameter :: D2R=acos(-1.0_dp)/180.0_dp

    real(kind=dp)
      :: angle, rise, run character(len=*),parameter :: all='(*(g0,1x))' ! given sine(theta) = 1.25 miles/50 miles
      (opposite/hypotenuse) ! then taking the arcsine of both sides of the equality yields ! theta = arcsine(1.25 miles/50
      miles) ie. arcsine(opposite/hypotenuse) rise=1.250_dp run=50.00_dp angle = asin(rise/run) print all, 'angle of
      incline(radians) = ', angle angle = angle/D2R print all, 'angle of incline(degrees) = ', angle

      print all, 'percent grade=',rise/run*100.0_dp end program demo_asin

  Results:

          angle of incline(radians) =    2.5002604899361139E-002
          angle of incline(degrees) =    1.4325437375665075
          percent grade=   2.5000000000000000

  The percentage grade is the slope, written as a percent. To calculate the slope you divide the rise by the run. In the example
  the rise is 1.25 mile over a run of 50 miles so the slope is 1.25/50 = 0.025.  Written as a percent this is 2.5 %.

  For the US, two and 1/2 percent is generally thought of as the upper limit. This means a rise of 2.5 feet when going 100 feet
  forward. In the US this was the maximum grade on the first major US railroad, the Baltimore and Ohio. Note curves increase the
  frictional drag on a train reducing the allowable grade.

STANDARD
  FORTRAN 77 and later, for a complex argument Fortran 2008 or later

SEE ALSO
  •  wikipedia: inverse trigonometric functions

  Inverse function: SIN(3)

  fortran-lang intrinsic descriptions (license: MIT) @urbanjost

                                                       September 04, 2022                                         asin(3fortran)
