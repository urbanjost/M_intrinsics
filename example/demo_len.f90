      program demo_len
      implicit none
      character(len=40) :: string
      character(len=:),allocatable :: astring
      character(len=:),allocatable :: many_strings(:)
      integer :: ii

         ii=len(string)
  write(*,*)'length =',ii

    ! the string length will be constant for the fixed-length variable string=' How long is this string? ' write(*,'(a)')'
    ',string,repeat('=',len(string))

    ! the allocatable string length will be the length of LHS expression astring=' How long is this string? ' write(*,'(a)')'
    ',astring,repeat('=',len(astring))

           ! you can also query the length (and other attributes) of a string
           ! using a "type parameter inquiry:" (available since fortran 2018)
           write(*,*)'length from type parameter inquiry=',string%len

           ! a scalar is returned for an array, as all values in a Fortran
           ! character array must be of the same length:

           ! define an allocatable array with a constructor ...
             many_strings = [ character(len=7) :: 'Takata', 'Tanaka', 'Hayashi' ]
           write(*,*)
           write(*,*)'length of ALL elements of array=',len(many_strings)

           call proc_star(' how long? ')

  contains

         subroutine proc_star(str)
         character(len=*),intent(in)  :: str
         character(len=:),allocatable :: str2
         ! the length of str can be used in the definitions of variables
         character(len=len(str))      :: str3

            if(allocated(str2))deallocate(str2)
            ! syntax for allocating a scalar string
            allocate(character(len=len(str)) :: str2)

            write(*,*)len(str),len(str2),len(str3)
            ! these are other allowable ways to define str2
            str2=str
            str2=repeat(' ',len(str))

    end subroutine proc_star

  end program demo_len
