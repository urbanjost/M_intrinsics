The only files that should require being edited are the text files ending
in *.md in the md?/ directores.

They require being in a format readable 
by the pandoc(1) script. Documentation is available by entering

   panddoc --help

This will convert the files to text in the txt/ document by entering
scripts/totxt.sh

This directory contains the tools required to convert the files in the
../md[357] directory:

   * to text in txt/
   * to manpages in ../man/man3 
   * as well as HTML pages in ../docs
   * and to extract the demo programs from the manpages and place them in ../test.

If you enter 

    make

all the output files should be generated. Addiionally to test the demo
program sources

    make demos

To see other options enter

    make help

It is assumed you are on a *nix system where bash(1), perl(1), groff(1),
make(1), man(1), pandoc(1) and (optionally) a recent gfortran(1) version are
installed.

Currently manserver(1) should be installed for converting the *roff files
to HTML files; as there are issues with groff(1).

You need to edit the Makefile in ../test to use compilers other than
gfortran(1) at the moment, but this should typically only require defining
a few variables in the Makefile.
