## atomic_cas

### **Name**

**atomic_cas**(3) - \[ATOMIC\] Atomically compare and swap a set of values

### **Synopsis**
```fortran
    call atomic_cas (atom, old, compare, new [,stat] )
```
```fortran
     subroutine atomic_cas (atom, old, compare, new, stat)
```
### **Characteristics**

### **Description**

**atomic_cas(atom, old, compare, new, stat)** atomically compares the
value of **atom** with **compare**. If they are equal, **atom** is set
to **new**, and **old** receives the previous value of **atom**. If not
equal, **atom** is unchanged, and **old** still receives the current
value of **atom**.

**atomic_cas** is useful for implementing locks or conditional updates.

Only one image’s **new** value is set if multiple images attempt the
operation simultaneously.

When **stat** is present and the invocation
was successful, it is assigned the value 0. If it is present and the
invocation has failed, it is assigned a positive value; in particular,
for a coindexed **atom**, if the remote image has stopped, it is assigned
the value of **iso_fortran_env**'s **stat_stopped_image** and if the remote
image has failed, the value **stat_failed_image**.

### **Options**



    STAT (optional): A scalar default-kind integer. Set to 0 on success,
    or a positive value on failure.
- **atom**
  : Scalar coarray or coindexed variable of either integer type with
  **atomic_int_kind** kind or logical type with kind **atomic_logical_kind**.

- **old**
  : Scalar of the same type and kind as **atom**.
    It receives the value of **atom** before the operation.

- **compare**
  : Scalar variable of the same type and kind as **atom**.
    Used for comparison.

- **new**
  : Scalar variable of the same type as **atom**. If kind is different, the
  value is converted to the kind of **atom**.
    It is given the new value from **atom** if the comparison succeeds.

- **stat**
  : (optional) Scalar default-kind integer variable.

### **Examples**

Sample program:

```fortran
program demo_atomic_cas_example
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: lock[*]
integer(atomic_int_kind) :: old
integer :: stat, me

  if (this_image() == 1) lock = 0
  sync all

  me = this_image()
  call atomic_cas(lock[1], old, 0, me, stat)

  if (stat /= 0) then
    print *, "Image", me, ": Failed with STAT =", stat
  else
    print *, "Image", me, ": Old =", old, ", New =", lock[1]
  end if
  sync all

  if (this_image() == 1) print *, "Final lock:", lock
end program demo_atomic_cas_example
```

Expected Output (4 images, order varies)

```text
    > Image 1: Old = 0, New = 1
    > Image 2: Old = 1, New = 1
    > Image 3: Old = 1, New = 1
    > Image 4: Old = 1, New = 1
    > Final lock: 1
```
### **Standard**

Fortran 2008 and later, per TS 18508

### **See Also**

[**atomic_define**(3)](#atomic_define),
[**atomic_ref**(3)](#atomic_ref),
[**iso_fortran_env**(3)](#)

See **iso_fortran_env** for constants like **atomic_int_kind**,
**stat_stopped_image**, and **stat_failed_image**.

 _Fortran intrinsic descriptions_
