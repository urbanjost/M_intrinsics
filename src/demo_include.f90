            program demo_include
            impicit none
            include "declarations.inc"
               write(*,*)'Hello World!'
               include "somecode.inc"
            includes
               include "somemorecode.inc"
            end program demo_include
