## flush

### **Name**
   flush(7f) - [FORTRAN:IO] flush I/O buffers of specified files
### **Synopsis**
```fortran
flush file-unit-number
flush([UNIT=]file_unit_number,[iostat=i],[iomsg=str],[err=label_number])
```
### **Description**

   I/O statements can buffer output before delivering it to the
   host system in order to minimize the overhead of system calls.
   Use **flush(7f)** to deliver any such pending I/O for the identified
   file to the host system.

   This is generally not required accept to ensure critical information
   is displayed or written as reliably as possible or to synchronise
   data from different units going to the same device. Do not arbitrarily
   flush all I/O operations or programs using large amounts of I/O might
   experience significant performance degradation, particularly if the
   I/O is to a block-oriented device.

   Note execution of a **flush(7f)** statement performs a wait operation
   for all pending asynchronous data transfer operations for the
   specified unit.

   More generally execution of a **flush(7f)** statement causes data
   written to an external file not only to be available to other
   processes,  causes data placed in an external file by means other
   than Fortran to be available to a **read(7f)** statement; but these
   actions are processor dependent.

   Execution of a **flush(7f)** statement for a file that is connected
   but does not exist is permitted and has no effect on any file.

   A **flush(7f)** statement has no effect on file position.

### **Options**
   UNIT    A file-unit-number is required; if the optional characters
           "UNIT=" are omitted, the unit-number must be the first
           item in the **flush(7)** statement.

### **Returns**
   **iostat**
   : status variable.
     It is set to a processor-dependent positive value if an
     error occurs, to zero if the flush operation was successful,
     or to a processor-dependent negative value if the flush
     operation is not supported for the unit specified.
   **iomsg**
   : character variable holding error description when iostat
     is not zero.
   **err**
   : The numeric line label of a target statement in the same
     scope as the **flush(7f)** statement.

   NOTE
   From the Fortran standard:

      Because the Fortran standard does not specify the mechanism of file
      storage, the exact meaning of the flush operation is not precisely
      defined. It is expected that the flush operation will make all data
      written to a file available to other processes or devices, or make data
      recently added to a file by other processes or devices available to
      the program via a subsequent read operation. This is commonly called
      flushing input/output buffers.

### **Examples**

Sample program:

```fortran
    program demo_flush
    use, intrinsic :: iso_fortran_env, only : &
    & stderr=>ERROR_UNIT, &
    & stdin=>INPUT_UNIT,  &
    & stdout=>OUTPUT_UNIT
    implicit none
    integer :: iostat
    character(len=255) :: iomsg
       flush (stderr, iostat=iostat, iomsg=iomsg)
       if(iostat.ne.0)then
          write(*,*)'ERROR:'//trim(iomsg)
          error stop 1
       endif
       flush (stdout, err = 999 )
       stop
       999 continue
       stop 10
    end program demo_flush
```
 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
