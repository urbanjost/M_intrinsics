program demo_kind
use, intrinsic :: iso_fortran_env, only : real32, real64, real128
implicit none
integer,parameter :: dc = kind(' ')
integer,parameter :: dl = kind(.true.)

! use number of digits in constant appropriate to the kind
! 
real(kind=real32),parameter :: pi4= &
real(3.14159265358979323846264338327950288419716939937_real128,kind(pi4))

real(kind=real64),parameter :: pi8= &
real(3.14159265358979323846264338327950288419716939937_real128,kind(pi8))

real(kind=real128),parameter :: pi16= &
real(3.14159265358979323846264338327950288419716939937_real128,kind(pi16))

real :: value
   print *, "The default character kind is ", dc
   print *, "The default logical kind is ", dl
   print *, '  &
   3.141592653589793238462643383279502884197169399375105820974944592307'
   print *, pi4
   print *, pi8
   print *, pi16

end program demo_kind
