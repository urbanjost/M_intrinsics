      program demo_random_init
         ! random_number(3f) on this invoking image will generate a sequence
         ! that differs form other images that invoke a similar statement, as
         ! well as being different on subsequent program execution.
         call random_init (repeatable=.false., image_distinct=.true.)

      end program demo_random_init
