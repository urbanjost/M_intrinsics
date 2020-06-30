           program demo_pack
           implicit none
           ! Sample program gathering nonzero elements from an array:
           call test1()
           ! Gathering nonzero elements from an array and appending elements from VECTOR:
           call test2()
           ! select strings
           call test3()
           contains
           !
           subroutine test1()
           integer :: m(6)
             m = [ 1, 0, 0, 0, 5, 0 ]
             write(*, fmt="(*(i0, ' '))") pack(m, m /= 0)  ! "1 5"
           end subroutine test1
           !
           subroutine test2()
           integer :: m(4)
             m = [ 1, 0, 0, 2 ]
             write(*, fmt="(*(i0, ' '))") pack(m, m /= 0, [ 0, 0, 3, 4 ])  ! "1 2 3 4"
           end subroutine test2
           !
           subroutine test3()
           character(len=10) :: m(4)
           m = [ character(len=10) :: 'ape', 'bat', 'cat', 'dog']
             write(*, fmt="(*(g0, ' '))") pack(m, m(:)(2:2) == 'a' )  ! "bat" "cat"
           end subroutine test3
           !
           end program demo_pack
