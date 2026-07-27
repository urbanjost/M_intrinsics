program testit
implicit none
integer  :: array(20,50)
integer  :: i, x, y, c

! fill an array with blocks of integer values that are an
! ADE (ASCII decimal equivalent of a printable character (ie. 32 to 126)
   array(:,:)=61 ! pre-fill array
! Fill array with rectangles of values
   array(  5:15,  4:45) = 43 
   array( 10:12,  3:35) = 45 
   array( 13:18,  2:45) = 45 
   array(  2:10, 26:49) = 45 
   array(  3: 8, 30:44) = 42 
   array(  2: 2,  2:14) = 45 
! print the array assuming the values can be printed as characters
   write (*, fmt = '(50a1)') (char(array(i,:)),i=1,size(array,dim=1))

! pick a point <10,17> and flood fill starting at the point with value 35
   x=10
   y=17
   c=array(10,17)
   call flood_fill(array,y,x,c,35)
   ! NOTE: DO NOT DO THIS INSTEAD:
   !   call flood_fill(array,y,x,array(10,17),35)
   ! you would be passing in a value that will be changed during the
   ! recursion!

! print the array to show the change
   write(*,'(a)')
   write (*, fmt = '(50a1)') (char(array(i,:)),i=1,size(array,dim=1))

contains

recursive subroutine flood_fill(array,y,x,old_attr,new_attr)
integer             ::  array(:,:)
integer,intent(in)  :: y, x, old_attr, new_attr
integer             :: test_attribute
! Stack-based recursive flood-fill (Four-way)
!
! Flood fill, also called seed fill, is an algorithm that determines the
! area connected to a given node in a multi-dimensional array. It is used
! in pixel-based graphics to "bucket" fill connected, similarly-colored
! areas with a different color,

! The flood fill algorithm takes three parameters: a start node, a
! target color, and a replacement color. The algorithm looks for all
! nodes in the array which are connected to the start node by a path of
! the target color, and changes them to the replacement color.

! Depending on whether we consider nodes touching at the corners connected
! or not, we have two variations, Eight-way and Four-way, respectively.

! One implicitly stack-based (recursive) flood-fill implementation
! (for a two-dimensional array) goes as follows:
!
! Flood-fill (node, target-color, replacement-color):
!  1. If target-color is equal to replacement-color, return.
!  2. If the color of node is not equal to target-color, return.
!  3. Set the color of node to replacement-color.
!  4. convert target-color to replacement-color by 
!     Perform Flood-fill (one step to the west of node) 
!     Perform Flood-fill (one step to the east of node) 
!     Perform Flood-fill (one step to the north of node) 
!     Perform Flood-fill (one step to the south of node) 
!  5. Return.
!
! Though easy to understand, the implementation of the algorithm above is
! impractical in languages and environments where stack space is severely
! constrained. Many other algorithms are available if this is an issue.
   test_attribute=array(y,x)
!     prevent loop changing A to A     Found something of the old color
   if(test_attribute.ne.new_attr .and. test_attribute.eq.old_attr)then
      array(y,x)=new_attr
      if(x.gt.1)                call flood_fill(array,y,x-1,old_attr,new_attr)
      if(x.lt.size(array,dim=2))call flood_fill(array,y,x+1,old_attr,new_attr)
      if(y.gt.1)                call flood_fill(array,y-1,x,old_attr,new_attr)
      if(y.lt.size(array,dim=1))call flood_fill(array,y+1,x,old_attr,new_attr)
   endif
end subroutine flood_fill

end program testit
