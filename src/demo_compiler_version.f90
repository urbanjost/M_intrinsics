          program demo_compiler_version
          use iso_fortran_env
          implicit none
          print '(4a)', &
             'This file was compiled by ', &
             compiler_version(),           &
             ' using the options ',        &
             compiler_options()
          end program demo_compiler_version
