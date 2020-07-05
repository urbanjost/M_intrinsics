#@(#) Makefile make *roff and html from markdown files using txt2man 2020-06-28 23:36:42

# you must have write permission to the install directory
MANDIR=/usr/share/man

FULLSRCS := $(wildcard ../md/*3fortran.man)
SRCS=$(notdir $(FULLSRCS))
VPATH=../md
VPATH=../man/man3
FULLMANS=$(patsubst %3fortran.man,../man/man3/%3fortran,$(SRCS))
FULLHTMLS=$(patsubst %3fortran.man,%3fortran.html,$(SRCS))

.SUFFIXES:
.SUFFIXES: $(SUFFIXES) .3fortran.man .3fortran .html

.phony : roff

all: roff
	bash scripts/book.sh FORTRAN fortran
	mandb -q -c ../man
	bash scripts/mank.sh 
	: checking for empty files in src/
	@for NAME in ../src/*.f90;\
	do\
	   [ -s "$$NAME" ] || echo $$(basename $$NAME .f90);\
	done|xargs -n 5|column -t
	: that is all folks!

roff: $(FULLMANS)
	: rebuilt manpages, html, and demo f90 programs

#-- rule for files in ../md ending in .man assumed to be txt2man(1) input 
#$(filter %.man,$(FULLMANS)) : ../man/man3/%.3fortran : ../md/%.3fortran.man
$(FULLMANS) : ../man/man3/%.3fortran : ../md/%.3fortran.man
	scripts/txt2man -s 3fortran -t "$*" $< |tee $@ |\
	bash scripts/man2html >../docs/$*.3fortran.html
	env MANPATH=../man bash scripts/man2f90 $*

.PHONY: install
install:
	: example of installing manpages on a system
	@test -w $(MANDIR)||(echo install : cannot write to $(MANDIR);exit 1)
	for NAME in $(FULLMANS); \
	do \
	   gzip --force --keep --stdout $$NAME > $(MANDIR)/$$(basename $$NAME).gz \
	   && chmod a=r,u+w $(MANDIR)/$$(basename $$NAME).gz; \
	done

.PHONY : demos
demos:
	: compile and load example programs from manpages
	cd ../demos;make

.PHONY : clean
clean:
	rm -f $(FULLMANS) 
	cd ../docs &&rm -f $(FULLHTMLS)
	rm ../ship/manpages.zip ../ship/manpages.tgz
	cd ../src/ && make clean

.PHONY : vi
vi:
	: test manpages with vim K command by editing manpage demo programs
#	vi -c 'set keywordprg=env\ MANPATH=$$MANPATH:../man::\ man\ -s\ 3fortran\ -a' ../src/*.f90
	: test K command in vim just using the manpages in ../man
	vim -c 'set keywordprg=env\ MANPATH=../man\ man\ -s\ 3fortran\ -a' ../src/*.f90

.PHONY : list
list:
	: list all the manpages and run a simplistic spellcheck
	env MANPATH=../man man -k .
	env MANWIDTH=80 MANPATH=../man man --regex '.*'|col -b
	env MANWIDTH=80 MANPATH=../man man --regex '.*'|bash scripts/spell.sh
        : to look up a word from the command line try
	: curl --silent dict://dict.org/d:"WORD:all"|more

.PHONY: help
help:
	@echo 'all      -- build all manpages, html pages, and write demo programs'
	@echo 'ship     -- rebuild archive files of manpages'
	@echo 'demos    -- build demo programs'
	@echo 'clean    -- remove output files built by this makefile'
	@echo 'list     -- list all manpages to stdout'
	@echo 'help     -- display this message'
	@echo 'install  -- install in $(MANDIR)'

.PHONY: distros
distros:
	: get utilities required to rebuild the distribution
	git clone https://github.com/mvertes/txt2man

.PHONY: ship
ship:
	: make easily distributed archive files of manpages
	cd ../man/man3;\
	   gzip *.3fortran;\
	   zip -r ../../ship/manpages.zip *gz;\
	   tar cvfz ../../ship/manpages.tgz *gz;\
	   gunzip *.3fortran.gz

.PHONY: debug
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
