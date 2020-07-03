

This directory contains the tools required to convert the files in the ../md
directory and generate manpages in ../man/man3 as well as HTML pages in ../docs
and to extract the demo programs from the manpages and place them in ../src.

If you enter 

    make

all the output files should be generated. Addiionally to test the demo program sources

    make demos

To see other options enter

    make help

It is assumed you are on a *nix system were bash(1), perl(1), groff(1), make(1), man(1)
and (optionally) a recent gfortran(1) version are installed.

Currently, you need to edit the Makefile in ../src to use compilers other than gfortran(1)
but this should typically only require defining a few variables in the Makefile.
