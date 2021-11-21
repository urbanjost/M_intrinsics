# NAME

**EXIT**(7f) - \[FORTRAN:EXECUTION CONTROL\] statement (LICENSE:PD)

# SYNOPSIS

**EXIT** \[construct-name\]

# DESCRIPTION

The **EXIT** statement provides a way of terminating a loop. It can also
complete execution of other constructs.

An **EXIT** statement cannot occur within or refer to a DO CONCURRENT
construct even if it refers to another construct.

If a *construct-name* appears, the **EXIT** statement must be within
that construct; otherwise, it has to be within the range of at least one
do-construct.

An **EXIT** statement belongs to a particular construct. If a construct
name appears, the **EXIT** statement belongs to that construct;
otherwise, it belongs to the innermost DO construct in which it appears.

When an **EXIT** statement that belongs to a DO construct is executed,
it terminates the loop and any active loops contained within the
terminated loop. When an **EXIT** statement that belongs to a non-DO
construct is executed, it terminates any active loops contained within
that construct, and completes execution of that construct.

# EXAMPLES

Samples:

``` 
   program demo_exit
   implicit none
   integer :: i, j
   logical :: big
      ! EXIT a simple loop
      do i=1, 10
         if(i .eq. 4) exit ! exit loop
      enddo
      write(*,*)'I=',i

      ! EXIT only exits an innermost loop
      do i=1,10
         do j=100, 200
            if(j .eq. 150) exit ! exit inner loop "j" but not "i"
         enddo
      enddo
      write(*,*)'I=',i,' J=',j

      ! EXIT an outermost loop from inner loop by using a construct name
      OUTER: do i=1,10
         INNER: do j=100, 200
            if(j .eq. 150) exit OUTER ! exit named loop "i"
         enddo INNER
      enddo OUTER
      write(*,*)'I=',i,' J=',j

      ! EXIT a BLOCK not just a DO loop
      MYBLOCK: block
         big = .false.
         do i = 1, 100
           if( i==40 )then
             exit MYBLOCK
           endif
         enddo
         big = .true.
      endblock MYBLOCK
      write(*,*)'I=',i,' BIG=',big
   end program demo_exit
```

Results:

``` 
    I=           4
    I=          11  J=         150
    I=           1  J=         150
    I=          40  BIG= F
```

## JSU
