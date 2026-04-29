## program

### **Name**

**program**(7) - \[PROGRAM UNITS\] Absolute value

### **Synopsis**
Basic Fortran program sections:
```fortran
    [ PROGRAM [program-name]]

        [ specification-part ]
        [ execution-part ]
        [ internal-subprogram-part ]

    end [PROGRAM [program-name]]
```
  A **program** directive optionally marks the beginning of a main
  program.

  A main program is the starting point for execution of a program.

  The main program may be defined by means other than Fortran; in that
  case, the program shall not contain a Fortran main-program program
  unit at all.

  The optional name of the main program has no explicit use within the
  Fortran language. It is available for documentation and for possible
  use by a processor.

  A processor might implement an _unnamed_ program unit by assigning it
  a global identifier that is not used elsewhere in the program. This
  could be done by using a default name that does not satisfy the rules
  for Fortran names, but if the name is specified it must conform to the
  rules for a Fortran identifier (composed from the ASCII alphanumeric
  characters and underscore, up to 63 characters, must begin with
  a letter).

  The **program** statement is optional but is almost always present in
  modern programs. Since it is optional a Fortran main program block is
  defined as a program unit that does not contain a SUBROUTINE, FUNCTION,
  MODULE, SUBMODULE,or BLOCKDATA statement as its first statement.

  Note that the **program** block is not required to be the first program
  unit in a file. Modules or procedure definitions or other program
  units may proceed it and most may follow it (A module must be defined
  before a reference to it is made).

  The program-name shall not be included in the end-program-stmt unless
  the optional program-stmt is used. If included, it shall be identical
  to the program-name specified in the program-stmt.

  NOTE1
  The program name is global to the program.

  An example of a main program is:
```fortran
            PROGRAM ANALYZE
                REAL A, B, C (10,10)              !   Specification part
                CALL FIND                         !   Execution part
            CONTAINS
                SUBROUTINE FIND                   !   Internal subprogram
                . . .
                END SUBROUTINE FIND
            END PROGRAM ANALYZE
```
  A reference to a Fortran main-program shall not appear in any program
  unit in the program, including itself. That is, you cannot call or
  jump to a main program from another program unit such as a module
  or procedure.

### **glossary**

program

   set of Fortran program units and entities defined by means other than
   Fortran that includes exactly one main program.

program unit

   A main program, external subprogram, module, submodule, or block data
   program unit.
