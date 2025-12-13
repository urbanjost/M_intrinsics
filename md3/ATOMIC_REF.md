## atomic_ref

### **Name**

**atomic_ref**(3) - \[ATOMIC\] Atomically retrieve the value in a variable

### **Synopsis**
```fortran
    call atomic_ref(value, atom [,stat] )
```
```fortran
     subroutine atomic_ref(value,atom,stat)

      integer(atomic_int_kind),intent(in) :: value
      integer(atomic_int_kind)            :: atom[*]
      integer,intent(out),intent(out)     :: stat
```
### **Characteristics**

- **atom** is a scalar coarray or coindexed variable of either integer
  type with atomic_int_kind kind or logical type with atomic_logical_kind
  kind.

- **value** is a scalar of the same type as **atom**. If the kind is
  different, the value is converted to the kind of **atom**.

- **stat** is a Scalar default-kind integer variable.

### **Description**

**atomic_ref(value, atom, stat)** atomically retrieves the value of **atom**
and stores it in **value**. This ensures a thread-safe read operation.

When **stat** is present and the invocation was successful, it is assigned
the value **0**. If it is present and the invocation has failed, it is
assigned a positive value; in particular, for a coindexed **atom**, if the
remote image has stopped, it is assigned the value of iso_fortran_env's
**stat_stopped_image** and if the remote image has failed, the value
**stat_failed_image**.

Use for safe reading of shared variables in parallel contexts.

It complements **atomic_define** for read-write operations.

### **Options**

- **value**
  : Receives the value of **atom**.
  : Scalar of the same type as **atom**. If the kind is different, the value
  is converted to the kind of **atom**.

- **atom**
  : Scalar coarray or coindexed variable of either integer type with
  **atomic_int_kind** kind or logical type with **atomic_logical_kind**
  kind.

- **stat**
  : (optional) Scalar default-kind integer variable.
  Set to 0 on success, or a positive value on failure.

### **Examples**

Sample program:

```fortran
program demo_atomic_ref
  use iso_fortran_env
  implicit none
  integer(atomic_int_kind) :: counter[*], value
  integer :: stat, me

  if (this_image() == 1) counter = 42
  sync all

  me = this_image()
  call atomic_ref(value, counter[1], stat)

  if (stat /= 0) then
    print *, "Image", me, ": Failed with STAT =", stat
  else
    print *, "Image", me, ": Retrieved value =", value
  end if
end program demo_atomic_ref
```
Expected Output (4 images, order varies)
```text
    > Image 1: Retrieved value = 42
    > Image 2: Retrieved value = 42
    > Image 3: Retrieved value = 42
    > Image 4: Retrieved value = 42
```
### **Standard**

Fortran 2008 ; with STAT, TS 18508

### **See Also**

[**atomic_define**(3)](#atomic_define),
[**atomic_cas**(3)](#atomic_cas),
[**iso_fortran_env**(3)](#),

[**atomic_fetch_add**(3)](#atomic_add),
[**atomic_fetch_and**(3)](#atomic_and),

[**atomic_fetch_or**(3)](#atomic_or),
[**atomic_fetch_xor**(3)](#atomic_xor)

See **iso_fortran_env** for constants like **atomic_int_kind**,
**stat_stopped_image**, and **stat_failed_image**.

 _Fortran intrinsic descriptions_
