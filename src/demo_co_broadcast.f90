          program demo_co_broadcast
            integer :: val(3)
            if (this_image() == 1) then
              val = [1, 5, 3]
            end if
            call co_broadcast (val, source_image=1)
            print *, this_image, ":", val
          end program demo_co_broadcast
