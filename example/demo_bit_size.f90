      program demo_bit_size
      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
      implicit none
      integer(kind=int64)          :: answer
      integer                      :: ilen
      character(len=*),parameter   :: fmt='(*(g0,1x))'
          write(*,fmt)'default integer size is',bit_size(0),'bits'
          write(*,fmt)bit_size(bit_size(0_int8)), 'which is kind=',kind(0_int8)
          write(*,fmt)bit_size(bit_size(0_int16)),'which is kind=',kind(0_int16)
          write(*,fmt)bit_size(bit_size(0_int32)),'which is kind=',kind(0_int32)
          write(*,fmt)bit_size(bit_size(0_int64)),'which is kind=',kind(0_int64)

          ! Check size of value not explicitly defined.
          write(*,fmt) int(bit_size(answer))
      end program demo_bit_size
