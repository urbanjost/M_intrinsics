## atomic_fetch_or

### **Name**

**atomic_fetch_or**(3) - \[ATOMIC:BIT MANIPULATION\] Atomically fetch
and perform a bitwise OR operation

### **Synopsis**
```fortran
    call atomic_fetch_or(atom, value, old [,stat] )
```
```fortran
     subroutine atomic_fetch_or(atom, value, old, stat)
```
### **Characteristics**

### **Description**

**atomic_fetch_or(atom, value, old, stat)** atomically stores the value
of **atom** in **old** and performs a bitwise **or** operation between
**atom** and **value**, storing the result in **atom**.

When **stat** is present and the invocation was successful,
it is assigned the value **0**. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
**atom**, if the remote image has stopped, it is assigned the value of
iso_fortran_env's stat_stopped_image and if the remote image has failed,
the value stat_failed_image.

The result is the bitwise **or** (e.g., 1000 OR 0011 = 1011).

It is useful for setting bit flags atomically.

### **Options**

- **atom**
  : Scalar coarray or coindexed variable of integer type with
  **atomic_int_kind** kind.

- **value**
  : Scalar of the same type as **atom**. If the kind is different,
  the value is converted to the kind of **atom**.

- **old**
  : Scalar of the same type and kind as **atom**.
  Receives the value of **atom** before the operation.

- **stat**
  : (optional) Scalar default-kind integer variable.
  Set to 0 on success, or a positive value on failure.

### **Examples**

Sample program:

```fortran
program demo_atomic_fetch_or
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: flags[*], old
integer :: stat, me

  if (this_image() == 1) flags = int(b'1000', atomic_int_kind)
  sync all

  me = this_image()
  call atomic_fetch_or(flags[1], int(b'0011', atomic_int_kind), old, stat)

  if (stat /= 0) print *, "Image", me, ": Failed with STAT =", stat
  print *, "Image", me, ": Old =", old
  sync all

  if (this_image() == 1) print *, "Final flags:", flags

end program demo_atomic_fetch_or
```
Expected Output (4 images, order varies)
```text
    > Image 1: Old = 8
    > Image 2: Old = 11
    > Image 3: Old = 11
    > Image 4: Old = 11
    > Final flags: 11
```

### **Standard**

TS 18508

### **See Also**

[**atomic_define**(3)](#atomic_define),
[**atomic_or**(3)](#atomic_or),
[**iso_fortran_env**(3)](#),

[**atomic_fetch_add**(3)](#atomic_fetch_add),
[**atomic_fetch_and**(3)](#atomic_fetch_and),

[**atomic_fetch_xor**(3)](#atomic_fetch_xor)

See **iso_fortran_env** for constants like **atomic_int_kind**,
**stat_stopped_image**, and **stat_failed_image**.

 _Fortran intrinsic descriptions_
