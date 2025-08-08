## atomic_fetch_and

### **Name**

**atomic_fetch_and**(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise
AND operation with prior fetch

### **Synopsis**
```fortran
    call atomic_fetch_and(atom, value, old [,stat] )
```
```fortran
     subroutine atomic_fetch_and(atom, value, old, stat)
```
### **Characteristics**

### **Description**

**atomic_fetch_and**(3) atomically fetches and performs a bitwise AND
operation.

It is similar to **atomic_and(3)**, but returns the previous value of
**atom**.  That is, it atomically stores the value of **atom** in **old**
and performs a bitwise **and** operation between **atom** and **value**,
storing the result in **atom**.

Useful for bit flag manipulation with feedback.

When **stat** is present and the invocation was successful, it is assigned
the value **0**. If it is present and the invocation has failed, it is
assigned a positive value; in particular, for a coindexed **atom**, if the
remote image has stopped, it is assigned the value of iso_fortran_env's
__stat_stopped_image__ and if the remote image has failed, the value
__stat_failed_image__.

### **Options**

- **atom**
  : Scalar coarray or coindexed variable of integer type with
  __atomic_int_kind__ kind.

- **value**
  : Scalar of the same type as **atom**. If the kind is different, the value
  is converted to the kind of **atom**.

- **old**
  : Scalar of the same type and kind as **atom**.
    Receives the value of ATOM before the operation.

- **stat**
  : (optional) Scalar default-kind integer variable.
    Set to 0 on success, or a positive value on failure.

### **Examples**

Sample program:

```fortran
program demo_atomic_fetch_and

  use iso_fortran_env
  implicit none
  integer(atomic_int_kind) :: flags[*], old
  integer :: stat, me

  if (this_image() == 1) flags = int(b'1111', atomic_int_kind)
  sync all

  me = this_image()
  call atomic_fetch_and(flags[1], int(b'1010', atomic_int_kind), old, stat)

  if (stat /= 0) print *, "Image", me, ": Failed with STAT =", stat
  print *, "Image", me, ": Old =", old
  sync all

  if (this_image() == 1) print *, "Final flags:", flags
end program demo_atomic_fetch_and
```
Expected Output (4 images, order varies)
```text
    > Image 1: Old = 15
    > Image 2: Old = 10
    > Image 3: Old = 10
    > Image 4: Old = 10
    > Final flags: 10
```
### **Standard**

Fortran 2008 and later, TS 18508

See **iso_fortran_env** for constants like **atomic_int_kind**,
**stat_stopped_image**, and **stat_failed_image**.

### **See Also**

[**atomic_define**(3)](#atomic_define),
[**atomic_and**(3)](#atomic_and),
[**iso_fortran_env**(3)](#),

[**atomic_fetch_add**(3)](#atomic_fetch_add),
[**atomic_fetch_or**(3)](#atomic_fetch_or),

[**atomic_fetch_xor**(3)](#atomic_fetch_xor)

See **iso_fortran_env** for constants like **atomic_int_kind**,
**stat_stopped_image**, and **stat_failed_image**.

 _Fortran intrinsic descriptions_
