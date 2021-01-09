program testit
use M_intrinsics, only : help_intrinsics
implicit none
character(len=132), allocatable :: manual(:)
integer                         :: i, j
integer                         :: count
character(len=30)               :: argument
    count=command_argument_count()
    if(count.eq.0)then
        manual = help_intrinsics('')
        write(*,'(g0)') ( trim(manual(i)), i=1, size(manual) )
    else
        do i=1, count
            call get_command_argument (count,argument) 
            manual = help_intrinsics(argument)
            write(*,'(g0)') ( trim(manual(j)), j=1, size(manual) )
        enddo
    endif
end program testit
