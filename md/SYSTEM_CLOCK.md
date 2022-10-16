## system_clock

### **Name**

**system_clock**(3) - \[SYSTEM:TIME\] Query system clock

### **Synopsis**
```fortran
    call system_clock([count] [,count_rate] [,count_max] )
```
```fortran
     subroutine system_clock(count, count_rate, count_max)

      integer,intent(out),optional  :: count
      type(TYPE(kind=KIND),intent(out),optional  :: count_rate
      integer,intent(out),optional  :: count_max
```
### **Characteristics**

 - **count** is an _integer_ scalar
 - **count_rate** an _integer_ or _real_ scalar
 - **count_max** an _integer_ scalar


### **Description**

  **system_clock**(3) lets you measure durations of time with the
  precision of the smallest time increment generally available on a
  system by returning processor-dependent values based on the current
  value of the processor clock.

  **system_clock** is typically used to measure short time intervals
  (system clocks may be 24-hour clocks or measure processor clock ticks
  since boot, for example). It is most often used for measuring or
  tracking the time spent in code blocks in lieu of using profiling tools.

  **count_rate** and **count_max** are assumed constant (even though
  CPU rates can vary on a single platform).

  Whether an image has no clock, has a single clock of its own, or shares
  a clock with another image, is processor dependent.

  If there is no clock, or querying the clock fails, **count** is set to
  **-huge(count)**, and **count_rate** and **count_max** are set to zero.

### **Options**

- **count**

  If there is no clock, **count** is returned as the negative value
  **-huge(count)**.

  Otherwise, the **clock** value is incremented by one for each clock
  count until the value **count_max** is reached and is then reset to
  zero at the next count. **clock** therefore is a modulo value that
  lies in the range **0 to count_max**.

- **count_rate** 
  : is assigned a processor-dependent approximation to the number of
  processor clock counts per second, or zero if there is no clock.
  **count_rate** is system dependent and can vary depending on the kind
  of the arguments. Generally, a large _real_ may generate a more precise
  interval.

- **count_max**
  : is assigned the maximum
  value that **COUNT** can have, or zero if there is no clock.

### **Examples**

  If the processor clock is a 24-hour clock that registers time at
  approximately 18.20648193 ticks per second, at 11:30 A.M. the reference

```fortran
      call system_clock (count = c, count_rate = r, count_max = m)
```
  defines
```text
      C = (11*3600+30*60)*18.20648193 = 753748,
      R = 18.20648193, and
      M = 24*3600*18.20648193-1 = 1573039.
```

Sample program:
```fortran
program demo_system_clock
implicit none
integer, parameter :: wp = kind(1.0d0)
integer :: count, count_rate, count_max
integer :: start, finish
real    :: time_read

   call system_clock(count, count_rate, count_max)
   write(*,*)'COUNT_MAX=',count_max
   write(*,*)'COUNT_RATE=',count_rate
   write(*,*)'CURRENT COUNT=',count

   call system_clock(start)
   ! <<<< code to time
   call system_clock(finish)

   time_read=(finish-start)/real(count_rate,wp)
   write(*,'(a30,1x,f7.4,1x,a)') 'time * : ', time_read, ' seconds'

end program demo_system_clock
```
Results:
```text
 >  COUNT_MAX=  2147483647
 >  COUNT_RATE=       10000
 >  CURRENT COUNT=   693921394
 >                      time * :   0.0000  seconds
```
### **Standard**

Fortran 95

### **See Also**

[**date_and_time**(3)](#date_and_time),
[**cpu_time**(3)](#cpu_time)

 _fortran-lang intrinsic descriptions_
