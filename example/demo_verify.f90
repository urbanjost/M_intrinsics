      program demo_verify
      implicit none
      character(len=*),parameter :: int='0123456789'
      character(len=*),parameter :: hex='abcdef0123456789'
      character(len=*),parameter :: low='abcdefghijklmnopqrstuvwxyz'
      character(len=*),parameter :: upp='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      character(len=20):: string='   Howdy There!'
      character(len=6) :: strings(2)=["Howdy ","there!"]
      character(len=2) :: sets(2)=["de","gh"]

         write(*,*)'first non-blank character ',verify(string, ' ')
         ! NOTE: same as len_trim(3)
         write(*,*)'last non-blank character',verify(string, ' ',back=.true.)

         ! first non-lowercase non-blank character
         write(*,*) verify(string,low//' ')

        ! elemental -- using arrays for both strings and for sets

         ! note character variables in an array have to be of the same length

         ! check each string from right to left for non-letter
         write(*,*) 'last non-letter',verify(strings,upp//low,back=.true.)

         ! find last non-uppercase character in "Howdy"
         ! and first non-lowercase in "There!"
         write(*,*) verify(strings,[upp,low],back=[.true.,.false.])

         write(*,*) verify("fortran", "", .true.)  ! 7, found 'n'
         ! 0' found none unmatched
         write(*,*) verify("fortran", "nartrof")

         ! first character in "Howdy" not in "de", and first letter in "there!"
         ! not in "gh"
         write(*,*) verify(strings,sets)

        ! check if string is of form NN-HHHHH
          CHECK : block
             logical                    :: lout
             character(len=80)          :: chars

             chars='32-af43d'
             lout=.true.

             ! are the first two characters integer characters?
             lout = lout.and.(verify(chars(1:2), int) == 0)

             ! is the third character a dash?
             lout = lout.and.(verify(chars(3:3), '-') == 0)

             ! is remaining string a valid representation of a hex value?
             lout = lout.and.(verify(chars(4:8), hex) == 0)

             if(lout)then
                write(*,*)trim(chars),' passed'
             endif

          endblock CHECK
      end program demo_verify
