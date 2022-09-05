      program demo_getenv
      implicit none
      character(len=:),allocatable :: homedir
      character(len=:),allocatable :: var
           var='HOME'
           homedir=get_env(var)
           write (*,'(a,"=""",a,"""")')var,homedir

      contains

      function get_env(NAME,DEFAULT) result(VALUE)
      ! a function that makes calling get_environment_variable(3) simple
      implicit none
      character(len=*),intent(in)          :: NAME
      character(len=*),intent(in),optional :: DEFAULT
      character(len=:),allocatable         :: VALUE
      integer                              :: howbig
      integer                              :: stat
      integer                              :: length
         ! get length required to hold value
         length=0
         VALUE=''
         if(NAME.ne.'')then
            call get_environment_variable( &
            & NAME, length=howbig,status=stat,trim_name=.true.)
            select case (stat)
            case (1)
             !*!print *, NAME, " is not defined in the environment. Strange..."
             VALUE=''
            case (2)
             !*!print *, &
             !*!"This processor does not support environment variables. Boooh!"
             VALUE=''
            case default
             ! make string to hold value of sufficient size
             if(allocated(VALUE))deallocate(VALUE)
             allocate(character(len=max(howbig,1)) :: VALUE)
             ! get value
             call get_environment_variable( &
             & NAME,VALUE,status=stat,trim_name=.true.)
             if(stat.ne.0)VALUE=''
            end select
         endif
         if(VALUE.eq.''.and.present(DEFAULT))VALUE=DEFAULT
      end function get_env

      end program demo_getenv
