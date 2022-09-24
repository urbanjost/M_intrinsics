      program demo_verify
      implicit none
      ! some useful character sets
      character,parameter :: &
       & int*(*)   = '1234567890', &
       & low*(*)   = 'abcdefghijklmnopqrstuvwxyz', &
       & upp*(*)   = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', &
       & punc*(*)  = "!""#$%&'()*+,-./:;<=>?@[\]^_`{|}~", &
       & blank*(*) = ' ', &
       & tab       = char(11), &
       & prnt*(*) = int//low//upp//blank//punc

      character(len=:),allocatable :: string
      integer :: i

         ! will produce the location of "d", because there is no match in UPP
         write(*,*) 'something unmatched',verify("ABCdEFG", upp)
         ! will produce 0 as all letters have a match
         write(*,*) 'everything matched',verify("ffoorrttrraann", "nartrof")

         ! easy C-like functionality but does entire strings not just characters
         write(*,*)'isdigit 123?',verify("123", int) == 0
         write(*,*)'islower abc?',verify("abc", low) == 0
         write(*,*)'isalpha aBc?',verify("aBc", low//upp) == 0
         write(*,*)'isblank aBc dEf?',verify("aBc dEf", blank//tab ) /= 0
         ! check if all printable characters
         string="aB;cde,fgHI!Jklmno PQRSTU vwxyz"
         write(*,*)'isprint?',verify(string,prnt) == 0
         ! this now has a nonprintable tab character in it
         string(10:10)=char(11)
         write(*,*)'isprint?',verify(string,prnt) == 0

         string=" This is NOT all UPPERCASE "
         write(*,*)'all uppercase/spaces?',verify(string, blank//upp) == 0
         string=" THIS IS ALL UPPERCASE "
         write(*,*) 'string=['//string//']'
         write(*,*)'all uppercase/spaces?',verify(string, blank//upp) == 0

        ! set and show complex string to be tested
         string='  Check this out. Let me know  '
         write(*,*) 'string=['//string//']'
         write(*,*) '        '//repeat(int,4) ! number line
         ! the Fortran functions result position just not a logical like C
         ! which can be very useful for parsing strings
         write(*,*)'first non-blank character',verify(string, blank)
         write(*,*)'last non-blank character',verify(string, blank,back=.true.)
         write(*,*)'first non-letter non-blank',verify(string,low//upp//blank)

        !VERIFY(3) is elemental so you can check an array of strings in one call
         ! are strings all letters (or blanks)?
         write(*,*) 'array of strings',verify( &
         ! strings must all be same length, so force to length 10
         & [character(len=10) :: "YES","ok","000","good one","Nope!"], &
         & low//upp//blank) == 0

         ! rarer, but the set can be an array, not just the strings to test
         ! you could do ISPRINT() this way :>
         write(*,*)'isprint?',.not.all(verify("aBc", [(char(i),i=32,126)])==1)

      end program demo_verify
      program demo_verify
      implicit none
      character(len=*),parameter :: &
        & int='0123456789', &
        & low='abcdefghijklmnopqrstuvwxyz', &
        & upp='ABCDEFGHIJKLMNOPQRSTUVWXYZ', &
        & blank=' '
      ! note character variables in an array have to be of the same length
      character(len=6) :: strings(3)=["Go    ","right ","home! "]
      character(len=2) :: sets(3)=["do","re","me"]

        ! elemental -- you can use arrays for both strings and for sets

         ! check each string from right to left for non-letter/non-blank
         write(*,*)'last non-letter',verify(strings,upp//low//blank,back=.true.)

         ! even BACK can be an array
         ! find last non-uppercase character in "Howdy "
         ! and first non-lowercase in "there "
         write(*,*) verify(strings(1:2),[upp,low],back=[.true.,.false.])

         ! using a null string for a set is not well defined. Avoid it
         write(*,*) 'null',verify("for tran ", "", .true.) ! 8,length of string?
         write(*,*) 'blank',verify("for tran ", " ", .true.) ! 7,found 'n'

         ! first character in  "Go    " not in "do",
         ! and first letter in "right " not in "ri"
         ! and first letter in "home! " not in "me"
         write(*,*) verify(strings,sets)

      end program demo_verify
