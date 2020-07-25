          program demo_atomic_ref
            use iso_fortran_env
            implicit none
            logical(atomic_logical_kind) :: atom[*]
            logical :: val
            call atomic_ref(atom, .false.)
            ! ...
            call atomic_ref(atom, val)
            if (val) then
              print *, "Obtained"
            end if
          end program demo_atomic_ref
