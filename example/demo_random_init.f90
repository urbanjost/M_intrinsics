  program demo_random_seed implicit none real x(3), y(3) call
  random_init(.true., .true.)  call random_number(x) call random_init(.true.,
  .true.)  call random_number(y) ! x and y should be the same sequence if (
  any(x /= y) ) stop "x(:) and y(:) are not all equal" end program
  demo_random_seed

SEE ALSO
  random_number, random_seed

  _fortran-lang intrinsic descriptions

                               February 27, 2023         random_init(3fortran)
