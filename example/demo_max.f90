      program demo_max
      implicit none
      real :: arr1(4)= [10.0,11.0,30.0,-100.0]
      real :: arr2(5)= [20.0,21.0,32.0,-200.0,2200.0]

        ! basic usage
         ! this is simple enough because all arguments are scalar
         write(*,*)'scalars:',max(10.0,11.0,30.0,-100.0)
         ! this is all max(3) could do before it became an elemental
         ! function and is the most intuitive
         ! except that it can take an arbitrary number of options,
         ! which is not common in Fortran without
         ! declaring a lot of optional parameters.
         !
         ! That is it unless you want to use the elemental features of max(3)!

        ! elemental
         ! there must be at least two arguments, so even if A1 is an array
         ! max(A1) is not valid. See MAXVAL(3) and/or MAXVAL(3) instead.

         ! If any argument is an array by the definition of an elemental
         ! function all the array arguments must be the same shape but
         ! MAXVAL([arr1, arr2]) or max(maxval(arr1),maxval(arr2))
         ! would work, for example.

         ! so an elemental call of two vectors does not return a single
         ! value, but the largest first element of the arrays, then the
         ! largest second element, and so on.
         write(*,*)max(arr1,arr2(1:4))
         ! multi-dimensional arrays are allowed, where the returned
         ! value will be an array of all the sets of the elements with
         ! the same coordinates.

         ! When mixing arrays and scalars you can think of the scalars
         ! as being a copy of one of the arrays with all values set to
         ! the scalar value ...
         write(*,*)'scalars and array:',max(10.0,11.0,30.0,-100.0,arr2)

         ! with two arrays and some scalars ...
         write(*,*)'scalars and array:',&
         & max(40.0,11.0,30.0,-100.0,arr2(:4),arr1)
      end program demo_max
