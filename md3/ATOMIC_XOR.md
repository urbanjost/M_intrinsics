## atomic_xor

### **Name**

**atomic_xor**(3) - \[ATOMIC:BIT MANIPULATION\] Atomically perform a
bitwise XOR operation

### **Synopsis**
```fortran
    call atomic_xor(atom, value [,stat] )
```
```fortran
     subroutine atomic_xor(atom,value,stat)

      integer(atomic_int_kind)            :: atom[*]
      integer(atomic_int_kind),intent(in) :: value
      integer,intent(out),intent(out)     :: stat
```
### **Characteristics**

- **atom** is a scalar coarray or coindexed variable of integer type with
  atomic_int_kind kind.

- **value** is a scalar of the same type as **atom**. If the kind is
  different, the value is converted to the kind of **atom**.

- **stat** is a Scalar default-kind integer variable.

### **Description**

**atomic_xor(atom, value, stat)** atomically performs a bitwise **xor**
operation between the value of **atom** and **value**, storing the result
in **atom**.

When **stat** is present and the invocation was successful, it is
assigned the value **0**. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
**atom**, if the remote image has stopped, it is assigned the value of
**iso_fortran_env**'s **stat_stopped_image** and if the remote image
has failed, the value **stat_failed_image**.

Unlike **atomic_fetch_xor**, this does not return the previous value.

Use for toggling bits atomically.

### **Options**

- **atom**
  : Scalar coarray or coindexed variable of integer type with
  atomic_int_kind kind.

- **value**
  : Scalar of the same type as **atom**. If the kind is different, the value
  is converted to the kind of **atom**.

- **stat**
  : (optional) Scalar default-kind integer variable.
  Set to 0 on success, or a positive value on failure.

### **Examples**

Sample program:

```fortran
program demo_atomic_xor
  use iso_fortran_env
  implicit none
  integer(atomic_int_kind) :: flags[*]
  integer :: stat, me

  if (this_image() == 1) flags = int(b'1100', atomic_int_kind)
  sync all

  me = this_image()
  call atomic_xor(flags[1], int(b'1010', atomic_int_kind), stat)

  if (stat /= 0) print *, "Image", me, ": Failed with STAT =", stat
  sync all

  if (this_image() == 1) print *, "Final flags:", flags
end program demo_atomic_xor
```
Expected Output (4 images)
```text
    > Final flags: 6
```
### **Standard**

TS 18508

### **See Also**

[**atomic_define**(3)](#atomic_define),
[**atomic_fetch_xor**(3)](#atomic_fetch),
[**iso_fortran_env**(3)](#),
[**atomic_add**(3)](#atomic_add),
[**atomic_or**(3)](#atomic_or),
[**atomic_xor**(3)](#atomic_xor)

See **iso_fortran_env** for constants like **atomic_int_kind**,
**stat_stopped_image**, and **stat_failed_image**.

 _Fortran intrinsic descriptions_
