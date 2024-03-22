         program demo_stop
         use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT
         implicit none
         integer :: stopcode
         character(len=:),allocatable :: message
         character(len=20)            :: which
         INFINITE: do
            ! Normal terminations
            write(*,'(a)')'enter a stop type:', &
                    & '{basic, text, zero, nonzero, variable, expression}', &
                    & '{error, errornum, errorstring}'
            read(*,'(a)')which
            select case(which)
               ! normal terminations:
               ! A STOP with no non-zero numeric parameter is a normal
               ! termination and generally returns a zero status value if the
               ! system supports return statuses
            case('basic'); stop    ! usually displays nothing
            case('zero');  stop 0  ! sometimes displays "STOP 0" or "0"
            case('text');  stop 'That is all, folks!'
               !
               ! All other stops are generally used to indicate an error or
               ! special exit type
            case('nonzero');                 stop 10
            case('variable'); stopcode=11;   stop stopcode
            case('expression'); stopcode=11; stop 110/stopcode
            case('string'); message='oops';  stop '<ERROR>:<'//message//'>'
               ! Error terminations:
               ! ERROR STOP is always an error stop, even without a stop-code
               ! ERROR STOP often displays a traceback but that is not required
            case('error')
               error stop
            case('errornum')
               stopcode=10
               error stop stopcode+3
            case('errorstring')
               message='That is all, folks!'
               error stop '<ERROR>'//message
            case default
               write(*,*)'try again ...'
            end select
         enddo INFINITE
         end program demo_stop
