      program demo_compiler_version
      use, intrinsic :: iso_fortran_env, only : compiler_version
      implicit none
            print '(4a)', 'This file was compiled by ', compiler_version()
      end program demo_compiler_version
