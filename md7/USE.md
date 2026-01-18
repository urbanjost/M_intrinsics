## use

### **Name**

**use**(7) - \[FORTRAN\] gives a program unit access to public entities
in a module.

### **Synopsis**
   There are two forms. One loads all public entities optionally
   renaming selected entities:
```fortran
    USE [, nature ::] module_name [, rename-list]
```
   The other makes accessible only explicitly named entities
```fortran
    USE [, nature ::] module_name, ONLY: only-list
```
### **Description**
   The **use** statement makes the entities in the specified module
   accessible to the current scoping unit. It also provides a means of
   renaming those entities with a rename-list and/or only importing a
   subset of the public entities from the module with an only-list.

   The entities accessed from the module may be named data objects,
   nonintrinsic types, procedures, abstract interfaces, generic
   identifiers, and namelist groups

   If more than one **use** statement appears in a scoping unit, the
   rename-lists are treated as one rename-list and the only-lists are
   treated as one concatenated only-list.

   At the time a USE statement is processed, the public portions of the
   specified module shall be available.  That is, the module typically
   needs to be compiled and found in the current search directories or
   previously in the current source files.

   A module shall not reference itself, either directly or indirectly.

   A **USE** statement without **only** provides access to all **public**
   entities in the specified namespace. This is true even if renamed
   entities are specified, but the renamed entities will only be available
   with the specified local name unless also explicitity requested with
   the original name. This is particularly useful behavior when there
   would otherwise be name conflicts.

   A **use** statement with **only** provides access only to those
   entities that appear in the only-list. It does not otherwise affect
   what is public due to a statement without **only**.

   **Remarks**

   A use-associated variable is considered to have been previously
   declared; any other use-associated entity is considered to have been
   previously defined.

   So accessed entities have their attributes
   (**type**,**kind**,...) specified via the module, except that
   an accessed entity may have a different accessibility attribute
   (eg. be declared **private** or **public**), it may be given the
   **asynchronous** attribute even if the associated module entity does
   not, and if it is not a coarray it may have the **volatile** attribute
   specified even if the associated entity from the module does not.

   If two or more generic interfaces that are accessible in the same
   scoping unit have the same name, same operator, or are assignments,
   they are interpreted as a single generic interface (that is, if there
   are no conflicts they are merged).

   Two or more accessible entities, other than generic interfaces, can
   have the same name only if no entity is referenced by this name in the
   scoping unit. That is, there can be no other conflicts unless the
   entities are not used.

   If local-name is absent, the use-name is available by use association.

   An entity can be accessed by more than one local-name.

   A local-name must not be declared with different attributes in the
   scoping unit that contains the USE statement, except that it can appear
   in a PUBLIC or PRIVATE statement in the scoping unit of a module.

   Forward references to modules are not allowed. That is, if a module is
   used in the same source file in which it resides, the module program
   unit must appear before its use.

   Definability of module entities can be controlled by the PROTECTED
   attribute.

   - OPERATOR (use-defined-operator) shall not identify a type-bound
     generic interface.
   - The generic-spec shall not identify a type-bound generic interface.

   These Constraints do not prevent accessing a generic-spec that is
   declared by an interface block, even if a type-bound generic interface
   has the same generic-spec.

   - An only-use-name shall be a nongeneric name.

   A USE statement with the ONLY option provides access only to
   those entities that appear as generic-specs, use-names, or
   use-defined-operators in the only-list.

   There is no prohibition against a use-name or use-defined-operator
   appearing multiple times in one USE statement or in multiple USE
   statements involving the same module. As a result, it is possible
   for one use-associated entity to be accessible by more than one
   local identifier.

   An entity in a scoping unit that is accessed by use association
   through more than one use path, has the ASYNCHRONOUS or VOLATILE
   attribute in any of those use paths, and is not given that attribute
   in that scoping unit, shall have that attribute in all use paths.

   the local-name is prohibited from appearing in a **common block** or
   an **equivalence** statement or a namelist-group-name in a NAMELIST
   statement, respectively. There is no prohibition against the local-name
   appearing as a common-block-name or a namelist-group-object.

### **Options**

 - **nature**
   : Is **intrinsic** or **non\_intrinsic**. If **intrinsic**
     is used, namespace must be the name of an intrinsic
     module. If **non_intrinsic** is used, namespace must
     be the name of an nonintrinsic module. If **nature**
     is not specified, a module of name namespace must be
     accessible. If both an intrinsic and non-intrinsic module
     of the same name exist and nature is not specified,
     the non-intrinsic module is used.

     It is an error to specify a user module and an intrinsic
     module of the same name in the same program unit.

 - **module\_name**
   : is a publicly accessible namespace; ie. it is the name
     of a module.

 - **rename-list**
   : is a comma-separated list of local-name => use-name.
 - **only-list**
   : is a comma-separated list of access-ids or
     [local-name => use-name]

     where

         local-name     Is the local name for the entity in the
                        program unit using the module or is
                        "OPERATOR (op-name)", where op-name is
                        the name of a defined operator in the
                        program unit using the module.
         use-name       is the name of a public entity in the
                        specified namespace
         access-id      is use-name or generic-spec
         generic-spec   is generic-name
                        or OPERATOR (defined-operator)
                        or ASSIGNMENT (=)

   **generic-name** is the name of a generic procedure

   **defined-operator** is one of the intrinsic operators
   or **.op-name.**

   **.op-name.** is a user-defined name for the operation

### **Examples**
 Samples:
```fortran
   ! program demo_use and module examples
   module example ! example is the namespace name
   use,intrinsic :: iso_fortran_env , only : real64

      type type1 ! type1 is the class prototype name
      contains
         procedure, nopass :: static_method1
      end type type1

      type type2 ! type1 is the class prototype name
      contains
         procedure, nopass :: static_method2
      end type type2

      real(kind=real64),parameter :: &
      pi  = 3.1415926535897932_real64
      ! Napier's constant is the base of the natural logarithm
      ! system. It is often denoted by "e" in honor of Euler.
      real(kind=real64),parameter :: &
      Napier_constant = 2.71828182845904523_real64

   contains

      subroutine static_method1(arg)
         integer :: arg
         ! code to implement method goes here
      end subroutine static_method1

      subroutine static_method2(arg)
         integer :: arg
         ! code to implement method goes here
      end subroutine static_method2

   end module example
   program demo_use
   use example, only: type1 ! class prototype type1 available,
                            ! but nothing else is made available by this
                            !
   ! (additionally) within this scoping unit, type1 is referred to
   ! as "mytype"
   use example, mytype => type1
   !
   ! only: is recommended but for long lists importing everything
   !       without listing it is supported:
   use example ! all public objects in namespace example available
   !
   ! some popular intrinsic entities
   !
   use,intrinsic :: iso_fortran_env, only : &
   stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
   ! specifying INTRINSIC or NON_INTRINSIC is typically optional but
   ! indicating INTRINSIC when it is so is the norm.
   use :: iso_fortran_env, only : integer_kinds,int8,int16,int32,int64
   use iso_fortran_env, only : real_kinds,real32,real64,real128
   ! duplicates are OK
   use,intrinsic :: iso_fortran_env, only : sp=>real32,dp=>real64
   use,intrinsic :: iso_fortran_env, only : integer_kinds
   use,intrinsic :: iso_fortran_env, only : compiler_version
   use,intrinsic :: iso_fortran_env, only : compiler_options
   use,intrinsic :: iso_fortran_env, only : iostat_eor, iostat_end
   end program demo_use
```
 **Subtle issues with multiple statements**

   As stated previously,
```text
   If more than one USE statement appears in a scoping unit, the
   rename-lists and only-lists are treated as one concatenated
   rename-list.
```
   That is, all the non-only statements are treated as one statement
   So this
```fortran
   use,intrinsic :: iso_fortran_env ! by itself would import all entities
   use,intrinsic :: iso_fortran_env, sp=>real32, dp=>real64
```
   is treated like this
```fortran
   use,intrinsic :: iso_fortran_env, sp=>real32, dp=>real64
```
   so the names **real32** and **real64** are not available. If you wanted
   both names you would have to add
```fortran
   use,intrinsic :: iso use,intrinsic , real32=>real32, real64=>real64
```
   or
```fortran
   use,intrinsic :: iso use,intrinsic , only: real32, real64
```
### **See Also**

[**private**(3)](#private),
[**public**(3)](#public),
[**module**(3)](#module)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
