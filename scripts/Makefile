#@(#) Makefile make *roff and html from markdown files using txt2man 2020-06-28 23:36:42

# you must have write permission to the install directory
MANDIR=/usr/share/man

FULLSRC3 := $(wildcard ../txt/*3fortran.man)
FULLSRC5 := $(wildcard ../txt/*5fortran.man)
FULLSRC7 := $(wildcard ../txt/*7fortran.man)
SRC3=$(notdir $(FULLSRC3))
SRC5=$(notdir $(FULLSRC5))
SRC7=$(notdir $(FULLSRC7))
VPATH=../txt
VPATH=../man/man3
VPATH=../man/man5
VPATH=../man/man7

FULLMAN3=$(patsubst %3fortran.man,../man/man3/%3fortran,$(SRC3))
FULLHTML3=$(patsubst %3fortran.man,%3fortran.html,$(SRC3))

FULLMAN5=$(patsubst %5fortran.man,../man/man5/%5fortran,$(SRC5))
FULLHTML5=$(patsubst %5fortran.man,%5fortran.html,$(SRC5))

FULLMAN7=$(patsubst %7fortran.man,../man/man7/%7fortran,$(SRC7))
FULLHTML7=$(patsubst %7fortran.man,%7fortran.html,$(SRC7))

FULLMANS=$(FULLMAN3) $(FULLMAN5) $(FULLMAN7)

.SUFFIXES:
.SUFFIXES: $(SUFFIXES) .3fortran.man .3fortran .html .5fortran.man .5fortran .7fortran.man .7fortran

.phony : roff

all: roff
	bash scripts/book.sh FORTRAN fortran
	mandb -q -c ../man
	bash scripts/mank.sh 
	: checking for empty files in example/
	@for NAME in ../example/*.f90;\
	do\
	   [ -s "$$NAME" ] || echo $$(basename $$NAME .f90);\
	done|xargs -n 5|column -t
	bash scripts/build_module.sh|expand|sed -e 's/  *$$//' > ../src/M_intrinsics.f90
	: that is all folks !

roff: $(FULLMANS)
	mandb ../man
	: rebuilt manpages, html, and demo f90 programs

#-- rule for files in ../txt ending in .man assumed to be txt2man(1) input 
#$(filter %.man,$(FULLMANS)) : ../man/man3/%.3fortran : ../txt/%.3fortran.man

$(FULLMAN3) : ../man/man3/%.3fortran : ../txt/%.3fortran.man
	scripts/txt2man -s 3fortran -t "$*" $< |tee $@ |\
	env AUX_FILENAME=$< bash scripts/man2html >../docs/$*.3fortran.html
	env MANWIDTH=80 man -Tutf8 --ascii --encoding=ascii -l $@ |col -b > ../text/$*.3fortran.man

	bash scripts/man2f90 $*

$(FULLMAN5) : ../man/man5/%.5fortran : ../txt/%.5fortran.man
	scripts/txt2man -s 5fortran -t "$*" $< |tee $@ |\
	env AUX_FILENAME=$< bash scripts/man2html >../docs/$*.5fortran.html
	env MANWIDTH=80 man -Tutf8 --ascii --encoding=ascii -l $@ |col -b > ../text/$*.3fortran.man
	bash scripts/man2f90 $*

$(FULLMAN7) : ../man/man7/%.7fortran : ../txt/%.7fortran.man
	scripts/txt2man -s 7fortran -t "$*" $< |tee $@ |\
	env AUX_FILENAME=$< bash scripts/man2html >../docs/$*.7fortran.html
	env MANWIDTH=80 man -Tutf8 --ascii --encoding=ascii -l $@ |col -b > ../text/$*.3fortran.man
	bash scripts/man2f90 $*

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
	cd ../example;make

.PHONY : clean
clean:
	rm -f $(FULLMANS) 
	cd ../docs &&rm -f $(FULLHTML3) $(FULLHTML7) $(FULLHTML5)
	rm -f ../docs/manpages3.zip ../docs/manpages3.tgz
	rm -f ../docs/manpages5.zip ../docs/manpages5.tgz
	rm -f ../docs/manpages7.zip ../docs/manpages7.tgz
	cd ../example/ && make clean

.PHONY : vi
vi:
	: test manpages with vim K command by editing manpage demo programs
#	vi -c 'set keywordprg=env\ MANPATH=$$MANPATH:../man::\ man\ -s\ 3fortran\ -a' ../example/*.f90
	: test K command in vim just using the manpages in ../man
	vim -c 'set keywordprg=env\ MANPATH=../man\ man\ -s\ 3fortran\ -a' ../example/*.f90

.PHONY : list
list:
	: list all the manpages and run a simplistic spellcheck
	env MANPATH=../man MANWIDTH= man -Tutf8 -k . 
	env MANWIDTH=80 MANPATH=../man man -Tutf8 --regex '.*'
	env MANWIDTH=80 MANPATH=../man man -Tutf8 --regex '.*'|col -b |bash scripts/spell.sh
	: 'to look up a word from the command line try'
	: 'curl --silent dict://dict.org/d:"WORD:all"|more'

.PHONY: help
help:
	@echo '         -- build changed manpages, html pages, and write demo programs'
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
	mkdir -p ../man/man3
	cd ../man/man3;\
	   gzip *.3fortran;\
	   zip -r ../../docs/manpages3.zip *gz;\
	   tar cvfz ../../docs/manpages3.tgz *gz;\
	   gunzip *.3fortran.gz
	cd ../man/man5;\
	   gzip *.5fortran;\
	   zip -r ../../docs/manpages5.zip *gz;\
	   tar cvfz ../../docs/manpages5.tgz *gz;\
	   gunzip *.5fortran.gz
	cd ../man/man7;\
	   gzip *.7fortran;\
	   zip -r ../../docs/manpages7.zip *gz;\
	   tar cvfz ../../docs/manpages7.tgz *gz;\
	   gunzip *.7fortran.gz

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
