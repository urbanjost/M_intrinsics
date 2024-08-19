   function subr(val)
   ! trivial function. What to note is 
   ! all the kinds are specified via "WP"
   real(kind=wp) :: subr
   real(kind=wp),intent(in) :: val
      subr=sqrt(val*3.0_wp)
   end function subr
