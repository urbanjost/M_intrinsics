  program demo_achar use,intrinsic::iso_fortran_env,only:int8,int16,int32,int64 implicit none integer :: i i=65

    write(*,'("decimal
      =",i0)')i

    write(*,'("character
      =",a1)')achar(i)

    write(*,'("binary
      =",b0)')achar(i)

    write(*,'("octal
      =",o0)')achar(i) write(*,'("hexadecimal =",z0)')achar(i)

                                                       September 04, 2022                                        achar(3fortran)
