## atomic_fetch_add

### **Name**

**atomic_fetch_add**(3) - \[ATOMIC\] Atomic fetch and add operation

### **Synopsis**
```fortran
    call atomic_fetch_add(atom, value, old [,stat] )
```
```fortran
     subroutine atomic_fetch_add(atom, value, old, stat)
```
### **Characteristics**

### **Description**

**atomic_fetch_add**(3) atomically stores the value of **atom** in **old**
and adds the value of **var** to the variable **atom**.

This operation is performed atomically, ensuring thread safety in parallel
environments, such as when using coarrays in Fortran for parallel
programming. It is part of the atomic operations defined in the Fortran
2008 standard and later, typically used with the **iso_fortran_env** module.

**atomic_fetch_add(3)** is useful in parallel programming to avoid race
conditions when multiple images update a shared variable.

The operation is only guaranteed to be atomic for variables of kind
**atomic_int_kind**.

For coindexed variables (e.g., counter[1]), the operation targets
the specified image’s coarray.

Always use synchronization (e.g., sync all) to ensure consistent
state across images before and after atomic operations.

When **stat** is present and the invocation was successful, it is
assigned the value **0**. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
**atom**, if the remote image has stopped, it is assigned the value of
**iso_fortran_env**'s **stat_stopped_image** and if the remote image
has failed, the value **stat_failed_image**.

### **Options**

- **atom**
  : Scalar coarray or coindexed variable of integer type with
    kind **atomic_int_kind** (from **iso_fortran_env**).

    Must be accessible across images in a parallel execution context.

- **value**
  : Scalar of the same type as **atom**. If the kind is different, the value
  is converted to the kind of **atom**.

- **old**
  : Scalar of the same type and kind as **atom**.

    On return, it contains the value of ATOM before the addition.

- **stat**
  : (optional) Scalar default-kind integer variable. If present:

        Set to 0 if the operation is successful.
        Set to a positive value if the operation fails (e.g.,
        STAT_STOPPED_IMAGE if the remote image has stopped, or
        STAT_FAILED_IMAGE if the remote image has failed, as defined
        in ISO_FORTRAN_ENV).

### **Examples**

The following program demonstrates the use of ATOMIC_FETCH_ADD in
a parallel context using coarrays. It increments a shared counter
atomically across multiple images and retrieves the original value before
the addition.

Sample program:

```fortran
program demo_atomic_fetch_add
  use iso_fortran_env
  implicit none
  integer(atomic_int_kind) :: counter[*]  ! Coarray for shared counter
  integer(atomic_int_kind) :: old_value   ! Stores value before addition
  integer :: stat, me, i

  ! Initialize counter on image 1
  if (this_image() == 1) counter = 0
  sync all  ! Ensure all images see initialized counter

  me = this_image()  ! Get current image number

  ! Each image atomically adds its image number to the counter
  call atomic_fetch_add(counter[1], me, old_value, stat)

  ! Check for errors
  if (stat /= 0) then
    print *, "Image", me, ": Operation failed with STAT =", stat
  else
    print *, "Image", me, ": Old value =", old_value, ", Added", me
  end if

  ! Synchronize all images before printing final result
  sync all

  ! Image 1 prints the final counter value
  if (this_image() == 1) then
    print *, "Final counter value:", counter
  end if
end program demo_atomic_fetch_add
```
### Explanation of Example

    Setup: The program uses the ISO_FORTRAN_ENV module to access
    ATOMIC_INT_KIND for the correct integer kind for atomic operations.

    Coarray: counter[*] is a coarray, allowing shared access across images
    (parallel processes).

    Initialization: Image 1 sets counter to 0, and sync all ensures all
    images see this initial value.

    Atomic Operation: Each image calls ATOMIC_FETCH_ADD to add its
    image number (me) to counter[1] (the counter on image 1), storing
    the value of counter[1] before the addition in old_value.

    Error Handling: The stat argument checks for operation success or failure.

    Output: Each image prints the value of counter[1] before its addition
    and the value added. Image 1 prints the final counter value after
    all operations.

### Expected Output

When run with 4 images (e.g., using cafrun -np 4 with a Fortran compiler
supporting coarrays, like gfortran), the output might look like (order
of image prints may vary due to parallelism):

```text
    > Image 1: Old value = 0, Added 1
    > Image 2: Old value = 1, Added 2
    > Image 3: Old value = 3, Added 3
    > Image 4: Old value = 6, Added 4
    > Final counter value: 10
```

The final counter value is the sum of image numbers (1 + 2 + 3 + 4 =
10), confirming atomic updates.


### **Standard**

TS 18508

### **See Also**

[**atomic_define**(3)](#atomic_define),
[**atomic_add**(3)](#atomic_add),
**iso_fortran_env**(3),

[**atomic_fetch_and**(3)](#atomic_fetch_and),
[**atomic_fetch_or**(3)](#atomic_fetch_or),

[**atomic_fetch_xor**(3)](#atomic_fetch_xor)

See **iso_fortran_env** for constants like **atomic_int_kind**,
**stat_stopped_image**, and **stat_failed_image**.

 _Fortran intrinsic descriptions_
