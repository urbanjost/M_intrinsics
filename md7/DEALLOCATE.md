## deallocate

### **Name**
  **deallocate**(7) - [FORTRAN:STATEMENT] causes allocated variables
  and targets to be deallocated

### **Synopsis**
  DEALLOCATE(allocate-object-list [,STAT=stat][,ERRMSG=errmsg] )

### **Description**
  The **deallocate** statement causes allocatable variables to be deallocated;
  it causes pointer targets to be deallocated and the pointers to be
  disassociated.

  An allocate-object shall not depend on the value, bounds, allocation
  status, or association status of another allocate-object in the same
  **deallocate** statement; it also shall not depend on the value of the
  stat-variable or errmsg-variable in the same **deallocate** statement.

  The status of objects that were not successfully allocated or
  deallocated can be individually checked with the intrinsic functions
  **allocated** or **associated**.

### **Options**

  **allocated-object-list**
  : Each allocate-object is a nonprocedure
  pointer or an allocatable variable.

  **STAT=stat-variable**
  : If the STAT= specifier appears, successful
  execution of the **allocate** or **deallocate**
  statement causes the stat-variable
  to become defined with a value of zero.

  If an error condition occurs during
  execution of a **deallocate** statement that
  does not contain the STAT= specifier, error
  termination is initiated.

  **ERRMSG=errmsg-variable**
  : If an error condition occurs during execution
  of an **allocate** or **deallocate** statement, the
  processor assigns an explanatory message to
  errmsg-variable. If no such condition occurs,
  the processor does not change the value of
  the errmsg-variable.

  No dealloc-opt shall appear more than once in a given **deallocate**
  statement.

  The errmsg-variable and stat-variable cannot be allocated or
  deallocated elsewhere in the statement or otherwise depend of any
  allocatable object in the statement.

### **Example**

   An example of a **deallocate** statement is:
```fortran
       DEALLOCATE (X, B)
```

### **Deallocation of allocatable variables**

   Deallocating an unallocated allocatable variable causes an error
   condition in the **deallocate** statement. Deallocating an allocatable
   variable with the TARGET attribute causes the pointer association
   status of any pointer associated with it to become undefined.

   When the execution of a procedure is terminated by execution of a
   RETURN or END statement, an unsaved allocatable local variable of
   the procedure retains its allocation and definition status if it
   is a function result variable or a subobject thereof; otherwise,
   it is deallocated.

   When a BLOCK construct terminates, an unsaved allocatable local
   variable of the construct is deallocated.

   If an executable construct references a function whose result is either
   allocatable or a structure with a subobject that is allocatable,
   and the function reference is executed, an allocatable result and
   any subobject that is an allocated allocatable entity in the result
   returned by the function is deallocated after execution of the
   innermost executable construct containing the reference.

   If a function whose result is either allocatable or a structure with
   an allocatable subobject is referenced in the specification part
   of a scoping unit or BLOCK construct, and the function reference
   is executed, an allocatable result and any subobject that is an
   allocated allocatable entity in the result returned by the function
   is deallocated before execution of the executable constructs of the
   scoping unit or block.

   When a procedure is invoked, any allocated allocatable object that
   is an actual argument corresponding to an INTENT (OUT) allocatable
   dummy argument is deallocated; any allocated allocatable object that
   is a subobject of an actual argument corresponding to an INTENT (OUT)
   dummy argument is deallocated.

   When an intrinsic assignment statement (7.2.1.3) is executed,
   any noncoarray allocated allocatable subobject of the variable is
   deallocated before the assignment takes place.

   When a variable of derived type is deallocated, any allocated
   allocatable subobject is deallocated.

   If an allocatable component is a subobject of a finalizable object,
   that object is finalized before the component is automatically
   deallocated.

   The effect of automatic deallocation is the same as that of a
   **deallocate** statement without a dealloc-opt-list.

   There is implicit synchronization of all images in association with
   each **deallocate** statement that deallocates one or more coarrays. On
   each image, execution of the segment (8.5.1) following the statement
   is delayed until all other images have executed the same statement
   the same number of times. If the coarray is a dummy argument, its
   ultimate argument (12.5.2.3) shall be the same coarray on every image.

   There is also an implicit synchronization of all images in association
   with the deallocation of a coarray or coarray subcomponent caused by
   the execution of a RETURN or END statement or the termination of a
   BLOCK construct.

   In the following example:

       > SUBROUTINE PROCESS
       >   REAL, ALLOCATABLE :: TEMP(:)
       >
       >   REAL, ALLOCATABLE, SAVE :: X(:)
       >   ...
       > END SUBROUTINE PROCESS

   on return from subroutine PROCESS, the allocation status of X is
   preserved because X has the SAVE attribute. TEMP does not have the
   SAVE attribute, so it will be deallocated if it was allocated. On
   the next invocation of PROCESS, TEMP will have an allocation status
   of unallocated.

### **Deallocation of pointer targets**

   If a pointer appears in a **deallocate** statement, its association status
   shall be defined. Deallocating a pointer
   that is disassociated or whose target was not created by an **allocate**
   statement causes an error condition in the **deallocate** statement. If a
   pointer is associated with an allocatable entity, the pointer shall
   not be deallocated.

   If a pointer appears in a **deallocate** statement, it shall be associated
   with the whole of an object that was
   created by allocation. Deallocating a pointer target causes the
   pointer association status of any other pointer that is associated
   with the target or a portion of the target to become undefined.

   If an **allocate** or **deallocate** statement with a coarray
   allocate-object is executed when one or more images has initiated
   termination of execution, the stat-variable becomes defined with the
   processor-dependent positive integer value of the constant STAT STOPPED
   IMAGE from the intrinsic module ISO_FORTRAN_ENV (13.8.2). If any other
   error condition occurs during execution of the **allocate** or **deallocate**
   statement, the stat-variable becomes defined with a processor-dependent
   positive integer value different from STAT STOPPED IMAGE. In either
   case, each allocate-object has a processor-dependent status:

   * each allocate-object that was successfully allocated shall have
     an allocation status of allocated or a pointer association
     status of associated;
   * each allocate-object that was successfully deallocated shall
     have an allocation status of unallocated or a pointer association
     status of disassociated;
   * each allocate-object that was not successfully allocated or
     deallocated shall retain its previous allocation status or
     pointer association status.
