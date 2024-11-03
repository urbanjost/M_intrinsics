## exit

### **Name**

**exit**(7f) - \[EXECUTION CONTROL\] terminate do-loops and block constructs

### **Synopsis**

**exit** \[construct-name\]

### **Description**

**exit* statements can terminate do-loops but also can exit most
named block constructs. First ...

   EXIT STATEMENTS CAN TERMINATE DO-LOOP CONSTRUCTS

The **exit** statement most commonly terminates a **do** or **do while**
loop.

The related **cycle** statement immediately begins the next loop cycle
versus terminating the loop.

An unnamed **exit** statement must be within a **do** loop and if
executed it exits the innermost **do** within which it appears,
terminating that loop.

If it is desired to exit nested do-loops the do-loop must be labeled
with a name, which the **exit** then refers to.

The **do** __control variables retain their last value.__ That is if a
**do** loop is terminated by and **exit** any additional inner **do**
loops are also terminated, but all **do loop** control variables retain
their last value; assuming they are still in scope.

   WITH A CONSTRAINT NAME EXIT CAN TERMINATE MOST BLOCK CONSTRUCTS

Named **exit** statements can also complete execution of other (named)
block constructs(eg. a **block** construct, an **associate** construct,
...).

If a construct name appears on an **exit**, the **exit** statement must
be within that construct. That is, an **exit** jumps to the end of the
associated construct only from within that very same construct.

If a non-**do** construct is terminated, any **do** loops inside that
construct are of course also terminated.

That stipulated, an **exit** statement can appear in any of the following
constructs:

   + **associate** construct
   + **block** construct
   + **if** construct
   + **select case** construct
   + **select rank** construct
   + **select type** construct
   + **do** construct
   + **changeteam** construct
   + **critical** construct

Note specifically what is missing -- **where** and **forall** constructs
cannot have **exit** statements associated with them.

A few additional restrictions apply, primarily for potentially parallel
regions.

 + An **exit** statement cannot cannot terminate a **do concurrent**
   construct because the execution order of the iterations is allowed
   to be indeterminate -- so an exit would result in an unknown state.

 + For related reasons jumping out of a parallel region to the exit
   of another block construct could skip steps that would leave a
   parallel program in an unknown state. Therefore **exit**
   statements in a **do concurrent**, **change team** or **critical**
   construct cannot reference an outer construct.

 + An exit from a **change team** construct does not just resume
   execution after the end of the construct. The effect is the same as
   transferring control to the **end team** statement, including that
   if that statement contains a __stat=__ or __errmsg=__ specifier,
   the __stat__ variable or __errmsg__ variable becomes defined.

### **Options**
   **construct-name**  (Optional for **do-loop** exits) Is the name of the
                       **do-loop** or block construct. Note the construct
                       names must be unique within the same scope.

   Unnamed **exit** statements could introduce errors when loop nesting
   is modified. Therefore names are strongly recommended accept perhaps
   where the loop comprises only a few lines of code.

### **Examples**
Samples:
```fortran
   program demo_exit
   implicit none
   integer,parameter :: arbitrary_size=10
   integer :: i, j, k, iarr(arbitrary_size)
   integer :: iostat, lun
   logical :: ok
   character(len=80) :: line
   character(len=*),parameter :: gen='(*(g0:,1x))'
   !
   ! the basics
   !
   ! Note we will use the function irand(3f) contained in
   ! the end of the code below to generate random whole numbers
   !
   !----------------------
   ! EXIT an infinite loop
   !----------------------
      i=0
      do
        i=i+1
        ! we will test on a random value to simulate an actual criteria
        ! to meet that indicates the loop should be terminated
        if(irand(-100,100).gt.95)exit
      enddo
      print gen, 'escaped infinite loop after only ',i,'tries'

     ! a related common use is to read a file of unknown size
     ! till an error or end-of-file, although READ does have
     ! the options ERR=numeric-label and EOF=numeric-label.
     ! INFINITE: do
     !    read(*,'(a)',iostat=iostat) line
     !    if(iostat.ne.0)exit INFINITE
     ! enddo INFINITE

   ! Some argue that an infinite loop is never a good idea.
   ! A common practice is to avoid even the possibility of an
   ! infinite loop by putting a cap on the number of iterations
   ! that should "never" occur, and then error processing
   ! if the unexpected number of loops is inadvertently reached.
   ! This technique can let your code gracefully handle being used with
   ! problems bigger than it was intended for, or not loop infinitely
   ! if some unexpected or incorrect input or condition is encountered.
   ! It might make it stop unitentionally as well.
     !
      ! run a loop but quit as soon as 200 random integers are odd
      j=0
      ! fun facts: What are the odds of not getting 200 in 10000?
      do i=1, 10000 
         k=irand(0,99)
         if((k+1)/2 /= k/2)j=j+1 ! cheap integer math trick to tell if odd
         if(j .ge. 200) exit
      enddo
      if(j.lt.200) then
         print gen,'Oh no! Not enough odd samples. only found',j
         print gen,'That is REALLY unlikely.'
         stop '<ERROR> unexpectedly low number of odd values'
      else
         print gen,'only did I=',i,'passes to get 200 odd samples'
      endif
   ! ---------------------------
   ! how to EXIT nested do-loops
   ! ---------------------------
     ! EXIT with no name only exits an innermost loop
     ! so in the following k will be 3, as all passes of the
     ! outer loop still occur
      k=0
      do i=1,3
         do j=1,5
            exit
         enddo
         k=k+1
      enddo
      ! at the end of a completed loop the counter is end_limit+step so
      ! you can tell if you exhausted the do loop or exited early:
      print gen,'I=',i,'so ',&
      & merge('completed','exited   ',i.gt.3),' outer loop'
      print gen,'J=',j,'so ',&
      & merge('completed','exited   ',j.gt.5),' inner loop'
      print gen,'K=',k

      ! COMMENTARY:
      ! A labeled exit is less prone to error so generally worth the
      ! additional verbosity even when just exiting an inner loop.
      ! Without a label an EXIT is somewhat like saying "EXIT SOMEWHERE".

   ! It is simple to EXIT nested loops from an inner loop.
   ! Just use a construct name. Lets start with the nested loop above
   ! that only repeatedly exited the inner loop and label the outer
   ! loop "OUTER". Now our exit can explicity name what loop it wants
   ! to exit ...

      k=0
      OUTER: do i=1,3
         do j=1,5
            exit OUTER
         enddo
         k=k+1
      enddo OUTER
      if(i==1.and.j==1.and.k==0)then
         print gen,'exited nested loops successfully as expected'
      else
         print gen,'something went wrong, i=',i,'j=',j,'k=',k
      endif

   ! ---------------------------------------
   ! exits from non-DO-loop block constructs
   ! ---------------------------------------
   ! REMEMBER: non-DO-loop exits are always named

   !----------------------------------------------------------------------
   ! EXIT a BLOCK statement surrounding a loop to avoid the nefarious GOTO
   !----------------------------------------------------------------------
      ! look for a 5 in an array that should always have it
      iarr=[(i,i=1,size(iarr))] ! fill array with 1 to N
      LOOKFOR: block
         do i=1,size(iarr)
           ! when you find what you are looking for use an EXIT instead
           ! of a GOTO , which follows much more restricted rules on
           ! on where you can land, preventing the threat of spaghetti code
           if(iarr(i).eq.5) exit LOOKFOR
         enddo
         write(*,*)'should not get here. iarr=',iarr
         stop '<INTERNAL ERROR> should never get here! is array too small?'
      endblock LOOKFOR
      print gen,'Good Found 5 at position I=',i,'so exited BLOCK construct'

   !--------------
   ! Dusty corners
   !--------------

   ! a block contained completely within a DO CONCURRENT can
   ! be exited even though the DO CONCURRENT itself or an outer block
   ! cannot be terminated from within a DO CONCURRENT
   do concurrent (i = 1:10)
      INCC:  block
        real :: t
       t = 0.0
       if (t == 0.0) exit INCC
       t= t+1.0
       end block INCC
   end do

   ! The following example shows illegal EXIT statements in DO CONCURRENT
   ! and CRITICAL:

   ! can   t EXIT DO CONCURRENT or outer construct of a DO CONCURRENT
   !x!N=4
   !x!LOOP_1 : DO CONCURRENT (I = 1:N)
   !x!  N = N + 1
   !x!  IF (N > I) EXIT LOOP_1
   !x!END DO LOOP_1

   !x!LOOP_2 : DO I = 1, 15
   !x!  CRITICAL
   !x!    N = N + 1
   !x!    IF (N > I) EXIT LOOP_2 ! cannott EXIT outer construct from inside
   !x!  END CRITICAL             ! CHANGE TEAM, DO CONCURRENT, or CRITICAL
   !x!END DO LOOP_2

   ! this would fail
   ! because the same construct name was used in the same scope:
   !x! LEVELA block:
   !x! exit LEVELA
   !x! endblock LEVELA
   !x!
   !x! LEVELA block:
   !x! exit LEVELA
   !x! endblock LEVELA

   contains
   ! choose a value from range of integers inclusive randomly
   function irand(first,last)
   integer, allocatable :: seed(:)
   integer,intent(in)   :: first,last
   real                 :: rand_val
   integer              :: irand
      call random_number(rand_val)
      irand = first + floor((last+1-first)*rand_val)
   end function irand
   end program demo_exit
```
Results:
```text
 > escaped infinite loop after only  71 tries
 > only did I= 426 passes to get 200 odd samples
 > I= 4 so  completed  outer loop
 > J= 1 so  exited     inner loop
 > K= 3
 > exited nested loops successfully as expected
 > Good Found 5 at position I= 5 so exited BLOCK construct
```
### **See Also**

 + [**cycle**(3)](#cycle)
 + [**return**(3)](#return)
 + [**stop**(3)](#stop)
 + [**do**(3)](#stop)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
