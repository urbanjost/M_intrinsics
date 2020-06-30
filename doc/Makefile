#@(#) Makefile make *roff and html from markdown files using txt2man 2020-06-28 23:36:42

SRCS := $(wildcard *3fortran.man)
MANSIMPLE=$(patsubst %3fortran.man,%3fortran,$(SRCS))
HTMLSIMPLE=$(patsubst %3fortran.man,%3fortran.html,$(SRCS))

MANDIR := ../man/
MANS=$(addprefix $(MANDIR)/man3/,$(MANSIMPLE) )

.SUFFIXES:
.SUFFIXES: $(SUFFIXES) .3fortran.man .3fortran .html

%3fortran.man: %.3fortran
	echo making $<

all: $(MANS)
	bash ../scripts/book.sh FORTRAN fortran
	mandb -q -c `pwd`/man
	: all done. Look in $(MANDIR)

$(MANS): $(SRCS)

%.3fortran:
	txt2man -s 3fortran -t "`basename $*`" `basename $*.3fortran.man` |tee $*.3fortran|\
	bash ../scripts/man2html >../docs/`basename $*`.3fortran.html
	bash ../scripts/man2f90 `basename $*`
	env MANPATH=$(MANDIR) man -s 3fortran `basename $*`

.PHONY : clean
clean:
	cd $(MANDIR)/man3 && rm -f $(MANSIMPLE) 
	cd ../docs/ && rm -f $(HTMLSIMPLE) 
	cd ../src/ && rm -f demo_*.f90
list:
	env MANPATH=$(MANDIR) man -s 3fortran -k .|cat -n|more
getlatest:
	: get txtman script from github
	git clone https://github.com/mvertes/txt2man.git
help:
	: all   -- update all manpages and force update of manpage index
	: clean -- remote built files
	: list  -- list manpages completed
	: help  -- show this index
