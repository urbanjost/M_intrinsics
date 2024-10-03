      program demo_pack
      implicit none
      integer, allocatable :: m(:)
      character(len=10) :: c(4)

       ! gathering nonzero elements from an array:
         m = [ 1, 0, 0, 0, 5, 0 ]
         write(*, fmt="(*(i0, ' '))") pack(m, m /= 0)

       ! Gathering nonzero elements from an array and appending elements
       ! from VECTOR till the size of the mask array (or array size if the
       ! mask is scalar):
         m = [ 1, 0, 0, 2 ]
         write(*, fmt="(*(i0, ' '))") pack(m, m /= 0, [ 0, 0, 3, 4 ])
         write(*, fmt="(*(i0, ' '))") pack(m, m /= 0 )

       ! select strings whose second character is "a"
         c = [ character(len=10) :: 'ape', 'bat', 'cat', 'dog']
         write(*, fmt="(*(g0, ' '))") pack(c, c(:)(2:2) == 'a' )

       ! creating a quicksort using PACK(3f)

   BLOCK
  INTRINSIC RANDOM_SEED, RANDOM_NUMBER REAL :: x(10) CALL RANDOM_SEED() CALL
  RANDOM_NUMBER(x) WRITE (*,"(a10,*(1x,f0.3))") "initial",x WRITE
  (*,"(a10,*(1x,f0.3))") "sorted",qsort(x)

   ENDBLOCK
   CONTAINS
  ! concise quicksort from @arjen and @beliavsky shows recursion, ! array
  sections, and vectorized comparisons.  PURE RECURSIVE FUNCTION qsort(values)
  RESULT(sorted) INTRINSIC PACK, SIZE REAL, INTENT(IN) :: values(:)

  REAL
    :: sorted(SIZE(values)) IF (SIZE(values) > 1) THEN sorted =
    [qsort(PACK(values(2:),values(2:)<values(1))), values(1), &
    qsort(PACK(values(2:),values(2:)>=values(1)))]

   ELSE
  sorted = values

   ENDIF
  END FUNCTION qsort end program demo_pack

  Result:

          > 1 5
          > 1 2 3 4
          > 1 2
          > bat        cat
          >    initial .833 .367 .958 .454 .122 .602 .418 .942 .566 .400
          >     sorted .122 .367 .400 .418 .454 .566 .602 .833 .942 .958

STANDARD
  Fortran 95

SEE ALSO
  MERGE(3), SPREAD(3), UNPACK(3)

  Fortran intrinsic descriptions (license: MIT) @urbanjost

                               October 02, 2024                 pack(3fortran)
