SRCS := $(wildcard *.f90)
PROGRAMS := $(patsubst %.f90,%,$(SRCS))
#Vary the command to your liking with make FC90=f90 F90FLAGS=-g  F90LDFLAGS=-s 

gfortran: FC90=gfortran 
gfortran: F90FLAGS= -fcoarray=single -Wall -Wextra -Wno-maybe-uninitialized -Wno-uninitialized -finit-real=snan
gfortran: F90LDFLAGS=
gfortran: all
	@echo built with $(F90)

ifx: FC90=ifx 
ifx: F90FLAGS=-warn all -coarray -traceback
ifx: F90LDFLAGS=
ifx: all
	@echo built with $(F90)

nvfortran: F90=nvfortran
nvfortran: F90FLAGS := -Minform=inform -Mbackslash -traceback
nvfortran: all
	@echo built with $(F90)

ifort: F90=ifort
ifort: F90FLAGS := -warn all -traceback
ifort: all
	@echo built with $(F90)

nagfor: F90=nagfor
nagfor: F90FLAGS := -C all
nagfor: all
	@echo built with $(F90)

# For this unusual case, where each executable has a single source file
# with a .f90 extension, all you need is 
.SUFFIXES: .f90

all: $(PROGRAMS)
#	echo $(SRCS)
#	echo $(PROGRAMS)
	: that is all, folks!

# The built-in default rules for make then work already
# Behind the scenes, make(1) is using the POSIX-mandated built-in single suffix rule

.f90:
	[ -s $< ] && $(FC90) $(F90FLAGS) $(LDFLAGS) $(TARGET_ARCH) -o $@ $< || echo $@ 

.phony : clean
clean:
	rm -f $(PROGRAMS) *.mod
