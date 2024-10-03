  program demo_inquire implicit none

  character(len=4096)
    :: filename

  character(len=20)
    :: mode

  integer
    :: ios

  character(len=256)
    :: message

  integer
    :: lun call print_inquire(lun,'') contains subroutine
    print_inquire(lun_in,namein_in)

  ! @(#) M_io print_inquire(3f) Do INQUIRE on file by name/number and print
  results

  integer,intent(in),optional
    :: lun_in        ! if unit >= 0 then query by unit number, else by name

  character(len=*),intent(in),optional
    :: namein_in

  integer
    :: ios

  character(len=256)
    :: message

  character(len=:),allocatable
    :: namein

  integer
    :: lun
    !==============================================================================================
    ! STATUS=NEW|REPLACE|OLD|SCRATCH|UNKNOWN

  character(len=20)
    :: access         ; namelist/inquire/access       ! SEQUENTIAL | DIRECT |
    STREAM

  character(len=20)
    :: asynchronous   ; namelist/inquire/asynchronous

  character(len=20)
    :: blank          ; namelist/inquire/blank

  character(len=20)
    :: decimal        ; namelist/inquire/decimal

  character(len=20)
    :: delim          ; namelist/inquire/delim

  character(len=20)
    :: direct         ; namelist/inquire/direct

  character(len=20)
    :: encoding       ; namelist/inquire/encoding

  logical
    :: exist          ; namelist/inquire/exist

  character(len=20)
    :: form           ; namelist/inquire/form          ! FORMATTED |
    UNFORMATTED

  character(len=20)
    :: formatted      ; namelist/inquire/formatted

  character(len=20)
    :: unformatted    ; namelist/inquire/unformatted

  integer
    :: id             ; namelist/inquire/id

  character(len=20)
    :: name           ; namelist/inquire/name

  logical
    :: named          ; namelist/inquire/named

  integer
    :: nextrec        ; namelist/inquire/nextrec

  integer
    :: number         ; namelist/inquire/number

  logical
    :: opened         ; namelist/inquire/opened

  character(len=20)
    :: pad            ; namelist/inquire/pad

  logical
    :: pending        ; namelist/inquire/pending

  integer
    :: pos            ; namelist/inquire/pos

  character(len=20)
    :: position       ; namelist/inquire/position       ! ASIS | REWIND |
    APPEND

  character(len=20)
    :: action         ; namelist/inquire/action         ! READ | WRITE |
    READWRITE

  character(len=20)
    :: read           ; namelist/inquire/read

  character(len=20)
    :: readwrite      ; namelist/inquire/readwrite

  character(len=20)
    :: write          ; namelist/inquire/write

  integer
    :: recl           ; namelist/inquire/recl

  character(len=20)
    :: round          ; namelist/inquire/round

  character(len=20)
    :: sequential     ; namelist/inquire/sequential

  character(len=20)
    :: sign           ; namelist/inquire/sign

  integer
    :: size           ; namelist/inquire/size

  character(len=20)
    :: stream         ; namelist/inquire/stream

         namein=merge_str(namein_in,'',present(namein_in))
         lun=merge(lun_in,-1,present(lun_in))
         ! exist, opened, and named always become defined unless an error condition occurs.
         !!write(*,*)'LUN=',lun,' FILENAME=',namein
         name=''
         if(namein == ''.and.lun /= -1)then
               call journal('sc','*print_inquire* checking unit',lun)
               inquire(unit=lun,                                                                               &
           &   recl=recl,nextrec=nextrec,pos=pos,size=size,                                                    &
           &   position=position,                                                                              &
           &   name=name,                                                                                      &
           &   form=form,formatted=formatted,unformatted=unformatted,                                          &
           &   access=access,sequential=sequential,direct=direct,stream=stream,                                &
           &   action=action,read=read,write=write,readwrite=readwrite,                                        &
           &   sign=sign,                                                                                      &
           &   round=round,                                                                                    &
           &   blank=blank,decimal=decimal,delim=delim,encoding=encoding,pad=pad,                              &
           &   named=named,opened=opened,exist=exist,number=number,pending=pending,asynchronous=asynchronous,  &
           &   iostat=ios,err=999,iomsg=message)
          elseif(namein /= '')then
               call journal('sc','*print_inquire* checking file:'//namein)
               inquire(file=namein,                                                                            &
           &   recl=recl,nextrec=nextrec,pos=pos,size=size,                                                    &
           &   position=position,                                                                              &
           &   name=name,                                                                                      &
           &   form=form,formatted=formatted,unformatted=unformatted,                                          &
           &   access=access,sequential=sequential,direct=direct,stream=stream,                                &
           &   action=action,read=read,write=write,readwrite=readwrite,                                        &
           &   sign=sign,                                                                                      &
           &   round=round,                                                                                    &
           &   blank=blank,decimal=decimal,delim=delim,encoding=encoding,pad=pad,                              &
           &   named=named,opened=opened,exist=exist,number=number,pending=pending,asynchronous=asynchronous,  &
           &   iostat=ios,err=999,iomsg=message)
           if(name == '')name=namein
          else
             call journal('sc','*print_inquire* must specify either filename or unit number')
          endif

         write(*,nml=inquire,delim='none')
         return

  999
    continue write(*,*)'*print_inquire* bad inquire'

  ! If an error condition occurs during execution of an INQUIRE  statement,

  ! all of the inquiry identifiers except ios become undefined.  write(*,*)
    '*print_inquire* inquire call failed,iostat=',ios,'message=',message end
    subroutine print_inquire end program demo_inquire

SEE ALSO
  BACKSPACE(7), CLOSE(7), ENDFILE(7), FLUSH(7), INQUIRE(7), OPEN(7), PRINT(7),
  READ(7), REWIND(7), WAIT(7), WRITE(7)

                               October 02, 2024              inquire(7fortran)
