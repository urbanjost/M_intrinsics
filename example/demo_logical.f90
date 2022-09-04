  program demo_logical ! Access array containing the kind type parameter values supported by this ! compiler for entities of
  logical type use iso_fortran_env, only : logical_kinds

         ! list kind values supported on this platform, which generally vary
         ! in storage size
         do i =1, size(logical_kinds)
            write(*,*)logical_kinds(i)
         enddo

  end program demo_logical
