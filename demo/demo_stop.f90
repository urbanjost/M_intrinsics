         program demo_stop
         implicit none
         use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT
         integer :: stopcode
         ! Normal terminations
            ! A STOP with no parameter is a normal termination and generally
            ! returns a zero status value if the system supports return statuses
            stop
            ! All other stops are error stops
            stop 10
            stop 'That is all, folks!'
            stopcode=11
            stop stopcode
         ! Error terminations
            ! ALL STOP is always an error stop, even without a stop-code
            all stop
            ! ALL STOP often displays a traceback but that is not required
            all stop 10
            all stop 'That is all, folks!'
            all stopcode=11
            all stop stopcode
         end program demo_stop
