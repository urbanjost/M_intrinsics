## atomic_and

### **Name**

**atomic_and**(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise AND operation

### **Synopsis**
```fortran
    call atomic_and(atom, value [,stat])
```
```fortran
     subroutine atomic_and(atom,value,stat)

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

**atomic_and(atom, value, stat)** atomically performs a bitwise **and**
operation between the value of **atom** and **value**, storing the result
in **atom**. This ensures thread-safe updates in parallel contexts.

Unlike **atomic_fetch_add**, this procedure does not return the previous
value of **atom**.

The result is the bitwise **and** of **atom** and **value**
(e.g., 1111 AND 1010 = 1010).

Useful for manipulating bit flags atomically.

Use sync all to ensure consistent coarray state across images.

When **stat** is present and the invocation was successful, it is
assigned the value 0. If it is present and the invocation has failed,
it is assigned a positive value; in particular, for a coindexed
**atom**, if the remote image has stopped, it is assigned the value of
**iso_fortran_env**'s **stat_stopped_image** and if the remote image
has failed, the value **stat_failed_image**.

### **Options**

- **atom**
  : Scalar coarray or coindexed variable of integer type with
  kind **atomic_int_kind** .

- **value**
  : Scalar of the same type as **atom**. If the kind is different,
  the value is converted to the kind of **atom**.

- **stat**
  : (optional) Scalar default-kind integer variable.
    Set to 0 on success, or a positive value on failure.

### **Examples**

Sample program:

```fortran
program demo_atomic_and
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: counter[*]
integer :: stat, me

  if (this_image() == 1) counter = 0
  sync all

  me = this_image()
  call atomic_add(counter[1], me, stat)

  if (stat /= 0) print *, "Image", me, ": Failed with STAT =", stat
  sync all

  if (this_image() == 1) print *, "Final counter:", counter
end program demo_atomic_and
```
Expected Output (4 images)
```text
    > Final counter: 10
```
### **Standard**

Fortran 2008 and later, per TS 18508

### **See Also**

[**atomic_fetch_and**(3)](#atomic_fetch_and),
[**atomic_define**(3)](#atomic_define),
[**atomic_ref**(3)](#atomic_ref),
[**atomic_cas**(3)](#atomic_cas),
**iso_fortran_env**(3),
[**atomic_add**(3)](#atomic_add),
[**atomic_or**(3)](#atomic_or),
[**atomic_xor**(3)](#atomic_xor)

See **iso_fortran_env** for constants like **atomic_int_kind**,
**stat_stopped_image**, and **stat_failed_image**.

 _Fortran intrinsic descriptions_
