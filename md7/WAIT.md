## wait

### **Name**
  wait(7f) - [IO] statement performs a wait operation for
  specified pending asynchronous data transfer operations
  
### **Synopsis**
```fortran
    WAIT( [UNIT=] file-unit-number,

     [END=label,]
     [EOR=label,]
     [ERR=label,]
     [ID=scalar-int-expr,]
     [IOMSG=iomsg-variable,] 
     [IOSTAT=scalar-int-variable]
     )
```
### **Description**
   The WAIT(7f) statement performs a wait operation for specified pending
   asynchronous data transfer operations.

   The CLOSE, INQUIRE, and file positioning statements may also perform
   wait operations.

   Execution of a WAIT statement specifying a unit that does not
   exist, has no file connected to it, or is not open for asynchronous
   input/output is permitted, provided that the WAIT statement has no ID=
   specifier; such a WAIT statement does not cause an error or end-of-file
   condition to occur.

   An EOR= specifier has no effect if the pending data transfer operation
   is not a nonadvancing read.

   An END= specifier has no effect if the pending data transfer operation
   is not a READ.

### **Options**
   No specifier shall appear more than once in a given wait-spec-list.

    [UNIT=]file-unit-number     A file-unit-number shall be specified
                                in a wait-spec-list; if the optional
                                characters UNIT= are omitted, the
                                file-unit-number shall be the first item
                                in the wait-spec-list.
    END=label, EOR=label, ERR=label   The label in the ERR=, EOR=,
                                      or END= specifier is a statement
                                      label of a branch target statement
                                      that appears in the same scoping
                                      unit as the WAIT statement.
    ID=scalar-int-expr   The value of the expression specified in
                         the ID= specifier shall be the identifier
                         of a pending data transfer operation for the
                         specified unit. If the ID= specifier appears, a
                         wait operation for the specified data transfer
                         operation is performed. If the ID= specifier is
                         omitted, wait operations for all pending data
                         transfers for the specified unit are performed.
    IOMSG=iomsg-variable  if IOSTAT is not zero, a corresponding message
                          describing the error
    IOSTAT=scalar-int-variable  status value indicating if an error occurred.
                                zero (0) indicates no error occurred.
### **Example**

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
