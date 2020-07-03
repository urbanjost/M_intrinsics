#@(#) Makefile make *roff and html from markdown files using txt2man 2020-06-28 23:36:42

FULLSRCS := $(wildcard ../md/*3fortran.man)
SRCS=$(notdir $(FULLSRCS))
VPATH=../md
VPATH=../man/man3

FULLMANS=$(patsubst %3fortran.man,../man/man3/%3fortran,$(SRCS))
FULLHTMLS=$(patsubst %3fortran.man,%3fortran.html,$(SRCS))
FULLF90S=$(patsubst %3fortran.man,%3fortran.html,$(SRCS))

.SUFFIXES:
.SUFFIXES: $(SUFFIXES) .3fortran.man .3fortran .html

.phony : roff

all: roff
	bash scripts/book.sh FORTRAN fortran
	mandb -q -c ../man
	bash scripts/mank.sh 
	: that is all folks!

roff: $(FULLMANS)
	: rebuilt manpages, html, and demo f90 programs

#-- rule for files in ../md ending in .man assumed to be txt2man(1) input 
#$(filter %.man,$(FULLMANS)) : ../man/man3/%.3fortran : ../md/%.3fortran.man
$(FULLMANS) : ../man/man3/%.3fortran : ../md/%.3fortran.man
	txt2man -s 3fortran -t "$*" $< |tee $@ |\
	bash scripts/man2html >../docs/$*.3fortran.html
	env MANPATH=../man bash scripts/man2f90 $*

.PHONY : demos
demos:
	cd ../src;make
.PHONY : clean
clean:
	rm -f $(FULLMANS) $(FULLHTMLS) 
	cd ../src/ && make clean
debug:
	@printf '%s', 'automatic variables
	:< $< 
	:@ $@ 
	:* $* 
	:@D $(@D) 
	:@F $(@F) 
	:*D $(*D) 
	:*F $(*F) 
	:%D $(%D) 
	:%F $(%F) 
	:<D $(<D) 
	:<F $(<F) 
	:^D $(^D) 
	:^F $(^F) 
	:+D $(+D) 
	:+F $(+F) 
	:?D $(?D) 
	:?F $(?F):'
