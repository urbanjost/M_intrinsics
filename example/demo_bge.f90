  program demo_bge use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64 implicit none

  integer
    :: i integer(kind=int8) :: byte integer(kind=int8),allocatable :: arr1(:), arr2(:) ! basic usage
    write(*,*)'bge(-127,127)=',bge( -127_int8, 127_int8 ) ! surprised -127 is great than 127 (on most machines, at least)?  ! if
    the values are not represented as "two's complement" or if ! the endian changes the representation of a sign can vary!

    write(*,*)'compare some values to 01000000 (64)' write(*,*)'Notice that the values are tested as bits, so essentially'
    write(*,*)'the values are tested as if unsigned integers.'  do i=-128,127,32 byte=i
    write(*,'(sp,i0.4,*(1x,1l,1x,b0.8))')i,bge(byte,64_int8),byte enddo ! elemental

           ! an array and scalar
           write(*, *)'compare array of values [-128, -0, +0, 127] to 127'
           write(*, *)bge(int([-128, -0, +0, 127], kind=int8), 127_int8)
           ! are +0 and -9 the same?

           ! two arrays
           write(*, *)'compare two arrays'
           arr1=int( [ -127, -0, +0,  127], kind=int8 )
           arr2=int( [  127,  0,  0, -127], kind=int8 )
           write(*,*)'arr1=',arr1
           write(*,*)'arr2=',arr2
           write(*, *)'bge(arr1,arr2)=',bge( arr1, arr2 )

    end program demo_bge
