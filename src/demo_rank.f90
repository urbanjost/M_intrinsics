          program demo_rank
            integer :: a
            real, allocatable :: b(:,:)
            real  :: c(10,20,30)
            print *, rank(a), rank(b), rank(c)
          end program demo_rank
