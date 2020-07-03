#@(#) Makefile make *roff and html from markdown files using txt2man 2020-06-28 23:36:42

FULLSRCS := $(wildcard *3fortran.man)
SRCS=$(notdir $(FULLSRCS))

MANS=$(patsubst %3fortran.man,%3fortran,$(SRCS))
HTMLS=$(patsubst %3fortran.man,%3fortran.html,$(SRCS))

.SUFFIXES:
.SUFFIXES: $(SUFFIXES) .3fortran.man .3fortran .html

.phony : roff

all: roff
#all: $(MANS)
#echo $(SRCS)
#bash ../scripts/book.sh FORTRAN fortran
#mandb -q -c `pwd`/man
#bash ../scripts/mank.sh 
#cd ../src;make -k
	: all done.

roff: $(MANS)
	echo roff

$(MANS) : %.3fortran : %.3fortran.man
	echo : $* : $@ :
	txt2man -s 3fortran -t "$*" $@.man |tee $@ |\
	bash ../scripts/man2html >../docs/$*.3fortran.html
	bash ../scripts/man2f90 $*

.PHONY : clean
clean:
	rm -f $(MANS) ../docs/$(HTMLS) 
	cd ../src/ && make clean
