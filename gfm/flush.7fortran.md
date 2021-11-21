# NAME

**flush**(7f) - \[FORTRAN:IO\] flush I/O buffers of specified files

# SYNOPSIS

**flush** *file-unit-number*

``` 
    or
```

**flush**(\[UNIT=\]file\_unit\_number,\[iostat=i\],\[iomsg=str\],\[err=label\_number\])

# DESCRIPTION

The actions of **FLUSH**(3f) are processor dependent because the Fortran
standard does not specify the mechanism of file storage. However, the
intention is

1.  The **FLUSH**(3f) operation should make all data written to an
    external file available to other processes or devices.

2.  It is also intended that it will cause data placed in an external
    file by means other than the current Fortran process to be available
    to the process in a subsequent READ statement.

Together, this is commonly called "flushing I/O buffers".

Note that execution of a **FLUSH**(3f) statement performs a wait
operation for all pending asynchronous data transfer operations for the
specified unit.

Execution of a **FLUSH**(3f) statement for a file that is connected but
does not exist is permitted and has no effect on any file.

A **FLUSH**(3f) statement has no effect on file position.

No specifier shall appear more than once in a given **FLUSH**(3f)
statement.

# OPTIONS

  - **\[UNIT=\]*file-unit-number***  
    Required. If the optional characters UNIT= are omitted from the unit
    specifier, the *file-unit-number* must be the first item.

  - **ERR=label**  
    The label must branch to a target statement in the same scoping unit
    as the **FLUSH**(3f) statement.

# RETURNS

  - **IOSTAT=scalar-int-variable**  
    variable is set to a processor-dependent positive value if an error
    occurs, to zero if the **flush** operation was successful, or to a
    processor-dependent negative value if the **flush** operation is not
    supported for the unit specified.

  - **IOMSG=iomsg-variable**  
    message describing any error that occurred

# EXAMPLE

Sample program:

``` 
   program demo_flush
   implicit none
   character(len=256) :: msg
   integer :: ios, lun
      lun=10
      flush (unit=lun, iostat=ios, iomsg=msg)
      if(ios.ne.0)then
         write(*,'(a)')'<ERROR>*flush*:'//trim(msg)
      endif
   end program demo_flush
```
