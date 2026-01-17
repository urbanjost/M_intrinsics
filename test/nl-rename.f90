! Does NAMELIST output respect USE renaming?

module m_namelist_output_respect_use_renaming
implicit none
  integer,save :: name_in_module = 123
end module m_namelist_output_respect_use_renaming

program test
use m_namelist_output_respect_use_renaming, &
 & name_in_program => name_in_module
implicit none
character(len=80) :: page(4)
integer :: i
integer :: iexit
namelist /nl/ name_in_program
   page=''
   iexit=0
   write(page,nl)
   do i=1,size(page)
      if(index(page(i),'NAME_IN_MODULE').ne.0)then
         write(*,*)'<FAILED> NAMELIST does not respect USE RENAMING'
         iexit=1
         exit
      elseif(index(page(i),'NAME_IN_PROGRAM').ne.0)then
         write(*,*)'<PASSED> NAMELIST respects USE RENAMING'
         exit
      endif
   enddo
   if(i.eq.size(page)+1)then
      write(*,*)'<ERROR> NAMELIST USE RENAMING test internal error'
      iexit=2
   endif
   stop iexit
end program test
