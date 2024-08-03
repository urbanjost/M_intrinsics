## stop

### **Name**

**stop**(7) - \[STATEMENT\] initiates termination of execution

### **Synopsis**
```fortran
stop [ stop-code ]
```
```fortran
error stop [ stop-code ]
```

### **Characteristics**

- **stop-code** is a constant scalar _character_or _integer_ expression,
  of default kind.

### **Description**

A *STOP* statement will cause the program to terminate normally.

It may provide additional information in the form of output or a system
status code, depending on the system.

Any messages generated appear on the ERROR\_UNIT file, as identified in
the intrinsic module ISO\_FORTRAN\_ENV. This unit is often referred to
as "stderr".

It is recommended that systems write the value of the stop code whether
numeric or a string.

Note that although *STOP* causes a "normal" termination,  system status
codes or "exit codes" are often used for error processing in many
scripting languages. This code may be detectable by
**EXECUTE\_SYSTEM\_COMMAND**(3f).

Execution of an **ERROR STOP** statement initiates error termination
*of* an execution, which on several systems includes the output from a
traceback.

So when an image is terminated by a **STOP** or **ERROR STOP** statement,
its stop code, if any, is made available in a processor-dependent manner.

If any exception is signaling on a stopped image, the processor issues
a warning indicating which exceptions are signaling;

When normal termination occurs on more than one image, it is expected
that a processor-dependent summary of any stop codes and signaling
exceptions will *be* made available.

If an integer **stop-code** is used as the process exit status, the
processor might be able to interpret only values within a limited
range, **or** only a limited portion of the integer value (for
example, only the least-significant 8 bits).

If the **stop-code** is of type character or does not appear,
**or** if an END PROGRAM statement is executed, it is recommended
that the value zero be supplied as the process exit status, if the
processor supports that concept.

### **Examples**

Sample:

```fortran
   program demo_stop
   use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT
   implicit none
   integer :: stopcode
   character(len=:),allocatable :: message
   character(len=20)            :: which
   INFINITE: do
      ! Normal terminations
      write(*,'(a)')'enter a stop type:', &
              & '{basic, text, zero, nonzero, variable, expression}', &
              & '{error, errornum, errorstring}'
      read(*,'(a)')which
      select case(which)
         ! normal terminations:
         ! A STOP with no non-zero numeric parameter is a normal
         ! termination and generally returns a zero status value if the
         ! system supports return statuses
      case('basic'); stop    ! usually displays nothing
      case('zero');  stop 0  ! sometimes displays "STOP 0" or "0"
      case('text');  stop 'That is all, folks!'
         !
         ! All other stops are generally used to indicate an error or
         ! special exit type
      case('nonzero');                 stop 10
      case('variable'); stopcode=11;   stop stopcode
      case('expression'); stopcode=11; stop 110/stopcode
      case('string'); message='oops';  stop 'ERROR:['//message//']'
         ! Error terminations:
         ! ERROR STOP is always an error stop, even without a stop-code
         ! ERROR STOP often displays a traceback but that is not required
      case('error')
         error stop
      case('errornum')
         stopcode=10
         error stop stopcode+3
      case('errorstring')
         message='That is all, folks!'
         error stop 'ERROR:'//message
      case default
         write(*,*)'try again ...'
      end select
   enddo INFINITE
   end program demo_stop
```

### **Standard**

   FORTRAN 77, ERROR STOP introduced in Fortran f2018

 _fortran-lang statement descriptions (license: MIT) \@urbanjost_
