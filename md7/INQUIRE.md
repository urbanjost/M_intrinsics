## inquire

### **Name**
   inquire(7f) - [FILE_INQUIRE] File inquiry statement
   
### **Synopsis**
```fortran
   INQUIRE([UNIT=file_unit_number]|[FILE=file_name_expr])

    and  ACCESS = scalar_default_char_variable
    or   ACTION = scalar_default_char_variable
    or   ASYNCHRONOUS = scalar_default_char_variable
    or   DIRECT = scalar_default_char_variable

    or   BLANK = scalar_default_char_variable
    or   DECIMAL = scalar_default_char_variable
    or   DELIM = scalar_default_char_variable
    or   ENCODING = scalar_default_char_variable
    or   PAD = scalar_default_char_variable
    or   SIGN = scalar_default_char_variable

    or   ERR = label
    or   IOMSG = iomsg_variable
    or   IOSTAT = scalar_int_variable

    or   EXIST = scalar_logical_variable
    or   FORM = scalar_default_char_variable
    or   FORMATTED = scalar_default_char_variable
    or   ID = scalar_int_expr
    or   NAME = scalar_default_char_variable
    or   NAMED = scalar_logical_variable
    or   NEXTREC = scalar_int_variable
    or   NUMBER = scalar_int_variable
    or   OPENED = scalar_logical_variable
    or   PENDING = scalar_logical_variable
    or   POS = scalar_int_variable
    or   POSITION = scalar_default_char_variable
    or   READ = scalar_default_char_variable
    or   READWRITE = scalar_default_char_variable
    or   RECL = scalar_int_variable
    or   ROUND = scalar_default_char_variable
    or   SEQUENTIAL = scalar_default_char_variable
    or   SIZE = scalar_int_variable
    or   STREAM = scalar_default_char_variable
    or   UNFORMATTED = scalar_default_char_variable
    or   WRITE = scalar_default_char_variable

     or

    INQUIRE(IOLENGTH=scalar_int_variable) output_item_list
```

### **Description**

  The INQUIRE statement may be used to inquire about properties of a
  particular named file or of the connection to a particular unit. There
  are three forms of the INQUIRE statement:

     o inquire by file, which uses the FILE= specifier
     o inquire by unit, which uses the UNIT= specifier
     o inquire by output list, which uses only the IOLENGTH= specifier.

  All specifier value assignments are performed according to the rules
  for assignment statements.

  For inquiry by unit, the unit specified need not exist or be connected
  to a file. If it is connected to a file, the inquiry is being made
  about the connection and about the file connected.

  An INQUIRE statement may be executed before, while, or after a file
  is connected to a unit. All values assigned by an INQUIRE statement
  are those that are current at the time the statement is executed.

  ERROR PROCESSING

  If an error condition occurs during execution of an INQUIRE statement,
  all of the inquiry specifier variables become undefined, except for
  variables in the IOSTAT= and IOMSG= specifiers (if any).

  The IOSTAT=, ERR=, and IOMSG= specifiers are described in 9.11.

### **Options**

  Unless constrained, the following inquiry specifiers may be used in
  either of the inquire by file or inquire by unit forms of the INQUIRE
  statement.

   o No specifier shall appear more than once in a given
     inquire_spec_list.

   o An inquire_spec_list shall contain one FILE= specifier or one UNIT=
     specifier, but not both.

   o In the inquire by unit form of the INQUIRE statement, if the optional
     characters UNIT= are omitted, the file_unit-number shall be the
     first item in the inquire_spec_list.

   o If an ID= specifier appears in an inquire_spec_list, a PENDING=
     specifier shall also appear.

   o The label in the ERR= specifier shall be the statement label of a
     branch target statement that appears in the same scoping unit as
     the INQUIRE statement.

  If file_unit-number identifies an internal unit, an error condition
  occurs.

  When a returned value of a specifier other than the NAME= specifier
  is of type character, the value returned is in upper case.

  The specifier that receives the returned value is a a default scalar
  variable.

### **Inputs**

   FILE

   The value of the file_name_expr in the FILE= specifier specifies the
   name of the file being inquired about. The named file need not exist
   or be connected to a unit. The value of the file_name_expr shall be
   of a form acceptable to the processor as a file name. Any trailing
   blanks are ignored. The interpretation of case is processor dependent.

   UNIT

### **Outputs**

   ACCESS

     SEQUENTIAL if the connection is for sequential access
     DIRECT     if the connection is for direct access
     STREAM     if the connection is for stream access.
     UNDEFINED  If there is no connection,

   ACTION

    READ       the connection is for input only,
    WRITE      the connection is for output only
    READWRITE  the connection is for both input and output.
    UNDEFINED  if there is no connection,

   ASYNCHRONOUS

    YES        if the connection allows asynchronous input/output
    NO         if the connection does not allow asynchronous input/output.
    UNDEFINED  If there is no connection

  BLANK

    ZERO       blanks are interpreted as zeros on input
    NULL       blanks are interpreted as a null on input
    UNDEFINED  no connection or the connection is not for formatted
               input/output

  DECIMAL

    COMMA      treat a comma as the separator between mantissa and decimal
    POINT      use a decimal point as the separator

  DELIM

    APOSTROPHE,  the delimiter mode in effect for a connection for
    QUOTE,       formatted input/output.
    NONE   

    UNDEFINED  no connection or the connection is not for formatted
               input/output


  DIRECT

    YES      if DIRECT is included in the set of allowed access methods
             for the file
    NO       if DIRECT is not included in the set of allowed access
             methods for the file
    UNKNOWN  if the processor is unable to determine whether DIRECT is
             included in the set of allowed access methods for the file.

  ENCODING

    UTF-8      if the connection is for formatted input/output with an
               encoding form of UTF-8
    UNDEFINED  the connection is for unformatted input/output.

    If there is no connection,

    UTF-8     if the processor is able to determine that the encoding
              form of the file is UTF-8
    UNKNOWN   if the processor is unable to determine the encoding
              form of the file

        NOTE
        The value assigned may be something other than UTF-8, UNDEFINED,
        or UNKNOWN if the processor supports other specific encoding forms
        (e.g. UTF-16BE).

  EXIST

   .true.    if there exists a file with the specified name if inquire
             is by FILE=filename statement or if by UNIT=number and the
             specified unit exists.

   .false.   otherwise, false is assigned.

  FORM

   FORMATTED    if the connection is for formatted input/output,
   UNFORMATTED  if the connection is for unformatted input/output.
   UNDEFINED    If there is no connection

  FORMATTED

   YES      if FORMATTED is included in the set of allowed forms for
            the file
   NO       if FORMATTED is not included in the set of allowed forms
            for the file
   UNKNOWN  if the processor is unable to determine whether FORMATTED is
            included in the set of allowed forms for the file.

  ID

   The value of the expression specified in the ID= specifier shall be
   the identifier of a pending data transfer operation for the specified
   unit. This specifier interacts with the PENDING= specifier.

  NAME

  The scalar_default_char_variable in the NAME= specifier is assigned
  the value of the name of the file if the file has a name; otherwise,
  it becomes undefined.

        NOTE
        If this specifier appears in an INQUIRE by file statement, its
        value is not necessarily the same as the name given in the FILE=
        specifier. However, the value returned shall be suitable for
        use as the value of the file_name_expr in the FILE= specifier
        in an OPEN statement.

        The processor may return a file name qualified by a user
        identification, device, directory, or other relevant information.

   The case of the characters assigned to scalar_default_char_variable
   is processor dependent.

   NAMED

   The scalar_logical_variable in the NAMED= specifier is assigned the
   value true if the file has a name; otherwise, it is assigned the
   value false.

   NEXTREC

   The scalar_int_variable in the NEXTREC= specifier is assigned the
   value n + 1, where n is the record number of the last record read
   from or written to the connection for direct access. If there is
   a connection but no records have been read or written since the
   connection, the scalar_int_variable is assigned the value 1. If
   there is no connection, the connection is not for direct access, or
   the position is indeterminate because of a previous error condition,
   the scalar_int_variable becomes undefined. If there are pending data
   transfer operations for the specified unit, the value assigned is
   computed as if all the pending data transfers had already completed.

   NUMBER

   The scalar_int_variable in the NUMBER= specifier is assigned the
   value of the external unit number of the unit that is connected to
   the file. If there is no unit connected to the file, the value -1
   is assigned.

   OPENED

   .true.   if the file specified is connected to a unit
   .false.  otherwise

   PAD

   YES,       corresponding to the pad mode in effect for a connection.
   NO

   UNDEFINED  

   If there is no connection or if the connection is not for
   formatted input/output,


   PENDING

   The PENDING= specifier is used to determine whether previously pending
   asynchronous data transfers are complete. A data transfer operation
   is previously pending if it is pending at the beginning of execution
   of the INQUIRE statement.

   If an ID= specifier appears and the specified data transfer operation
   is complete, then the variable specified in the PENDING= specifier
   is assigned the value false and the INQUIRE statement performs the
   wait operation for the specified data transfer.

   If the ID= specifier is omitted and all previously pending data
   transfer operations for the specified unit are complete, then the
   variable specified in the PENDING= specifier is assigned the value
   false and the INQUIRE statement performs wait operations for all
   previously pending data transfers for the specified unit.

   In all other cases, the variable specified in the PENDING= specifier
   is assigned the value true and no wait operations are performed;
   in this case the previously pending data transfers remain pending
   after the execution of the INQUIRE statement.

        NOTE:
        The processor has considerable flexibility in defining when
        it considers a transfer to be complete. Any of the following
        approaches could be used:

           o The INQUIRE statement could consider an asynchronous data
             transfer to be incomplete until after
             the corresponding wait operation. In this case PENDING=
             would always return true unless there were no previously
             pending data transfers for the unit.

           o The INQUIRE statement could wait for all specified data
             transfers to complete and then always return
             false for PENDING=.

           o The INQUIRE statement could actually test the state of the
             specified data transfer operations.

  POS

  The scalar_int_variable in the POS= specifier is assigned the number
  of the file storage unit immediately following the current position of
  a file connected for stream access. If the file is positioned at its
  terminal position, the variable is assigned a value one greater than
  the number of the highest-numbered file storage unit in the file. If
  the file is not connected for stream access or if the position of
  the file is indeterminate because of previous error conditions, the
  variable becomes undefined.

  POSITION

  The scalar_default_char_variable in the POSITION= specifier
  is assigned the value REWIND if the connection was opened for
  positioning at its initial point, APPEND if the connection was opened
  for positioning before its endfile record or at its terminal point,
  and ASIS if the connection was opened without changing its position.
  If there is no connection or if the file is connected for direct
  access, the scalar_default_char_variable is assigned the value
  UNDEFINED. If the file has been repositioned since the connection, the
  scalar_default_char_variable is assigned a processor-dependent value,
  which shall not be REWIND unless the file is positioned at its initial
  point and shall not be APPEND unless the file is positioned so that
  its endfile record is the next record or at its terminal point if it
  has no endfile record.

  READ

  The scalar_default_char_variable in the READ= specifier is assigned
  the value YES if READ is included in the set of allowed actions for
  the file, NO if READ is not included in the set of allowed actions for
  the file, and UNKNOWN if the processor is unable to determine whether
  READ is included in the set of allowed actions for the file.

  READWRITE

  The scalar_default_char_variable in the READWRITE= specifier is
  assigned the value YES if READWRITE is included in the set of allowed
  actions for the file, NO if READWRITE is not included in the set of
  allowed actions for the file, and UNKNOWN if the processor is unable to
  determine whether READWRITE is included in the set of allowed actions
  for the file.

  RECL

  The scalar_int_variable in the RECL= specifier is assigned the value of
  the record length of a connection for direct access, or the value of
  the maximum record length of a connection for sequential access. If
  the connection is for formatted input/output, the length is the
  number of characters for all records that contain only characters of
  default kind. If the connection is for unformatted input/output, the
  length is measured in file storage units. If there is no connection,
  or if the connection is for stream access, the scalar_int_variable
  becomes undefined.

  ROUND

  The scalar_default_char_variable in the ROUND= specifier is assigned
  the value UP, DOWN, ZERO, NEAREST, COMPATIBLE, or PROCESSOR DEFINED,
  corresponding to the I/O rounding mode in effect for a connection for
  formatted input/output. If there is no connection or if the connection
  is not for formatted input/output, the scalar_default_char_variable
  is assigned the value UNDEFINED. The processor shall return the value
  PROCESSOR DEFINED only if the behavior of the current I/O rounding
  mode is different from that of the UP, DOWN,
  ZERO, NEAREST, and COMPATIBLE modes.

  SEQUENTIAL

  The scalar_default_char_variable in the SEQUENTIAL= specifier is
  assigned the value YES if SEQUENTIAL is included in the set of allowed
  access methods for the file, NO if SEQUENTIAL is not included in the set
  of allowed access methods for the file, and UNKNOWN if the processor
  is unable to determine whether SEQUENTIAL is included in the set of
  allowed access methods for the file.

  SIGN

  The scalar_default_char_variable in the SIGN= specifier is assigned the
  value PLUS, SUPPRESS, or PROCESSOR DEFINED, corresponding to the sign
  mode in effect for a connection for formatted input/output. If there is
  no connection, or if the connection is not for formatted input/output,
  the scalar_default_char_variable is assigned the value UNDEFINED.

  SIZE

  The scalar_int_variable in the SIZE= specifier is assigned the size of
  the file in file storage units. If the file size cannot be determined,
  the variable is assigned the value -1.

  For a file that may be connected for stream access, the file size is
  the number of the highest-numbered file storage unit in the file.

  For a file that may be connected for sequential or direct access, the
  file size may be different from the number of storage units implied by
  the data in the records; the exact relationship is processor-dependent.

  STREAM

  The scalar_default_char_variable in the STREAM= specifier is assigned
  the value YES if STREAM is included in the set of allowed access
  methods for the file, NO if STREAM is not included in the set of
  allowed access methods for the file, and UNKNOWN if the processor is
  unable to determine whether STREAM is included in the set of allowed
  access methods for the file.

  UNFORMATTED

  The scalar_default_char_variable in the UNFORMATTED= specifier is
  assigned the value YES if UNFORMATTED is included in the set of allowed
  forms for the file, NO if UNFORMATTED is not included in the set of
  allowed forms for the file, and UNKNOWN if the processor is unable to
  determine whether UNFORMATTED is included in the set of allowed forms
  for the file.

  WRITE

  The scalar_default_char_variable in the WRITE= specifier is assigned
  the value YES if WRITE is included in the set of allowed actions for
  the file, NO if WRITE is not included in the set of allowed actions
  for the file, and UNKNOWN if the processor is unable to determine
  whether WRITE is included in the set of allowed actions for the file.

INQUIRE BY OUTPUT LIST

  The scalar_int_variable in the IOLENGTH= specifier is assigned the
  processor-dependent number of file storage units that would be required
  to store the data of the output list in an unformatted file. The
  value shall be suitable as a RECL= specifier in an OPEN statement
  that connects a file for unformatted direct access when there are
  input/output statements with the same input/output list.

  The output list in an INQUIRE statement shall not contain any
  derived-type list items that require a defined input/output procedure
  as described in subclause 9.6.3. If a derived-type list item appears
  in the output list, the value returned for the IOLENGTH= specifier
  assumes that no defined input/output procedure will be invoked.

### **Examples**
  Examples of INQUIRE statements are:
```fortran
     INQUIRE (IOLENGTH = IOL) A (1:N)
     INQUIRE (UNIT = JOAN, OPENED = LOG_01, NAMED = LOG_02, &
        FORM = CHAR_VAR, IOSTAT = IOS)
```
### **Examples**
Sample program:
```fortran
program demo_inquire
implicit none
integer :: lun=40
integer :: iostat
   write(*,*)'is it open or predefined?'
   call print_inquire(lun,'')
   write(*,*)'what are the defaults?'
   open(unit=lun)
   call print_inquire(lun,'')
   close(unit=lun,status='delete',iostat=iostat)
contains
subroutine print_inquire(lun_in,filename) 

! @(#) print_inquire(3f) print INQUIRE of file by name/number

integer,intent(in),optional           ::  lun_in
character(len=*),intent(in),optional  ::  filename
integer                               ::  iostat
character(len=256)                    ::  message
character(len=:),allocatable          ::  filename_ 
integer                               ::  lun
! STATUS=NEW|REPLACE|OLD|SCRATCH|UNKNOWN
! SEQUENTIAL | DIRECT | STREAM | UNDEFINED
character(len=20)  ::  access        ;  namelist/inquire/access  
character(len=20)  ::  asynchronous  ;  namelist/inquire/asynchronous                                    
character(len=20)  ::  blank         ;  namelist/inquire/blank                                           
character(len=20)  ::  decimal       ;  namelist/inquire/decimal                                         
character(len=20)  ::  delim         ;  namelist/inquire/delim                                           
character(len=20)  ::  direct        ;  namelist/inquire/direct                                          
character(len=20)  ::  encoding      ;  namelist/inquire/encoding                                        
!  FORMATTED   |  UNFORMATTED     
character(len=20)  ::  form          ;  namelist/inquire/form         
character(len=20)  ::  formatted     ;  namelist/inquire/formatted                                       
character(len=20)  ::  unformatted   ;  namelist/inquire/unformatted                                     
character(len=20)  ::  name          ;  namelist/inquire/name                                            
character(len=20)  ::  pad           ;  namelist/inquire/pad                                             
!  ASIS        |  REWIND       |  APPEND
character(len=20)  ::  position      ;  namelist/inquire/position 
!  READ        |  WRITE        |  READWRITE
character(len=20)  ::  action        ;  namelist/inquire/action   
character(len=20)  ::  read          ;  namelist/inquire/read                                            
character(len=20)  ::  readwrite     ;  namelist/inquire/readwrite                                       
character(len=20)  ::  write         ;  namelist/inquire/write                                           
character(len=20)  ::  round         ;  namelist/inquire/round                                           
character(len=20)  ::  sequential    ;  namelist/inquire/sequential                                      
character(len=20)  ::  sign          ;  namelist/inquire/sign                                            
character(len=20)  ::  stream        ;  namelist/inquire/stream                                          
integer            ::  id            ;  namelist/inquire/id                                              
integer            ::  nextrec       ;  namelist/inquire/nextrec                                         
integer            ::  number        ;  namelist/inquire/number                                          
integer            ::  pos           ;  namelist/inquire/pos                                             
integer            ::  recl          ;  namelist/inquire/recl                                            
integer            ::  size          ;  namelist/inquire/size                                            
logical            ::  exist         ;  namelist/inquire/exist                                           
logical            ::  named         ;  namelist/inquire/named                                           
logical            ::  opened        ;  namelist/inquire/opened                                          
logical            ::  pending       ;  namelist/inquire/pending                                         

   if(present(filename))then
      filename_ =filename
   else
      filename_ =''
   endif
   lun=merge(lun_in,-1,present(lun_in))
   ! exist, opened, and named always become defined 
   ! unless an error condition occurs.
   if(filename_  == ''.and.lun /= -1)then
     write(*,*)'*print_inquire* checking unit',lun
     inquire(unit=lun,recl=recl,nextrec=nextrec,pos=pos,size=size,      &
     & position=position,name=name,form=form,formatted=formatted,       &
     & unformatted=unformatted,access=access,sequential=sequential,     &
     & direct=direct,stream=stream,action=action,read=read,write=write, &
     & readwrite=readwrite,sign=sign,round=round,blank=blank,           &
     & decimal=decimal,delim=delim,encoding=encoding,pad=pad,           &
     & named=named,opened=opened,exist=exist,number=number,             &
!bug & pending=pending,                                                 & 
     & asynchronous=asynchronous,                                       &
     & iostat=iostat,err=999,iomsg=message)
   elseif(filename_  /= '')then
     write(*,*)'*print_inquire* checking file:'//filename_ 
     inquire(file=filename_ ,recl=recl,nextrec=nextrec,pos=pos,         &
     & size=size,position=position,name=name,                           &
     & form=form,formatted=formatted,unformatted=unformatted,           &
     & access=access,sequential=sequential,direct=direct,stream=stream, &
     & action=action,read=read,write=write,readwrite=readwrite,         &
     & sign=sign,round=round,blank=blank,decimal=decimal,delim=delim,   &
     & encoding=encoding,pad=pad,named=named,opened=opened,exist=exist, &
     & number=number,pending=pending,asynchronous=asynchronous,         &
     & iostat=iostat,err=999,iomsg=message)
   else
      write(*,*)'*print_inquire* must specify either filename or unit number'
   endif
   write(*,nml=inquire,delim='none')
   return
999   continue
   write(*,*)'*print_inquire* bad inquire'
!  If an error condition occurs during execution of an INQUIRE  statement,
!  all of the inquiry identifiers except iostat become undefined.
   write(*,*) '*print_inquire* inquire call failed,iostat=',iostat, &
   & 'message=',message
end subroutine print_inquire
end program demo_inquire
```
### **See Also**

[**backspace**(7)](#backspace),
[**close**(7)](#close),
[**endfile**(7)](#endfile),
[**flush**(7)](#flush),
[**inquire**(7)](#inquire),
[**open**(7)](#open),
[**print**(7)](#print),
[**read**(7)](#read),
[**rewind**(7)](#rewind),
[**wait**(7)](#wait),
[**write**(7)](#write)
