## random_init

### **Name**

**random_init**(3) - \[MATHEMATICS:RANDOM\] Initializes the state of
the pseudorandom number generator

### **Synopsis**
```fortran
    call random_init(repeatable, image_distinct)

     logical,intent(in) :: repeatable
     logical,intent(in) :: image_distinct
```
### **Characteristics**

- **harvest** and **image_distinct** are logical scalars

### Description

Initializes the state of the pseudorandom number generator used by
**random_number**.

### **Options**

**repeatable**
: If it is **.true.**, the seed is set to a processor-dependent
value that is the same each time **random_init** is called from the
same image. The term "same image" means a single instance of program
execution. The sequence of random numbers is different for repeated
execution of the program.

If it is **.false.**, the seed is set to a processor-dependent value.

**image_distinct**
: If it is `.true.`, the seed is set to a processor-dependent value that
is distinct from the seed set by a call to **random_init**in another
image. If it is **.false.**, the seed is set to a value that does depend
on which image called **random_init**.

### **Examples**

Sample program:

```fortran
    program demo_random_init
    implicit none
    real x(3), y(3)
       call random_init(.true., .true.)
       call random_number(x)
       call random_init(.true., .true.)
       call random_number(y)
       ! x and y should be the same sequence
       if ( any(x /= y) ) stop "x(:) and y(:) are not all equal"
    end program demo_random_init
```
### **Standard**

Fortran 2018

### **See also**

[random_number](#random_number),
[random_seed](#random_seed)

 _Fortran intrinsic descriptions
