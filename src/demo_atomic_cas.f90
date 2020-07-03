          program demo_atomic_cas
            use iso_fortran_env
            logical(atomic_logical_kind) :: atom[*], prev
            call atomic_cas(atom[1], prev, .false., .true.)
          end program demo_atomic_cas
