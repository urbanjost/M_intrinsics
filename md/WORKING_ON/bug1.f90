program wrong_array
implicit none
real, target, allocatable     :: big(:,:)
real, pointer, dimension(:,:) :: p1
   allocate(big(600,700))
   p1 => big
   if(associated(p1))then
      if(allocated(p1))then
         write(*,*)size(p1)
         write(*,*)lbound(p1)
         write(*,*)ubound(p1)
      endif
   endif
   deallocate (p1)  ! NO: indirect deallocation is not allowed
   write(*,*)allocated(big)
   if(allocated(big))then
      write(*,*)size(big)
      write(*,*)lbound(big)
      write(*,*)ubound(big)
   endif
end program wrong_array
