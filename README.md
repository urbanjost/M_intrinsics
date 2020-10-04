## NAME

   # INTRINSICS_PROJECT(7f) - [FORTRAN] intrinsic manpages

## DESCRIPTION

This is a project to provide a set of man(1) pages for the standard
Fortran intrinsics, with a secondary goal of providing a tested working
example program for each intrinsic.

Unlike many parts of a language, intrinsics can naturally be described
in discrete units using manpages. If we get enough momentum going here
using a "divide and conquer" approach, a finished product can quickly
be produced and transferred to

   [https://fortran-lang.org](https://fortran-lang.org)

in an HTML format as well.

The man(1) interface is available for all *nix systems and Cygwin as well
as other platforms; and is used to view documentation on-line in a CLI
(Command Line Interface) environment.

These documents are at the state of "good enough considering the
alternative is nothing", but are still actively being edited.

The manpages are available as archive files:

   - [manpages.tgz](ship/manpages.zip)
   - [manpages.zip](ship/manpages.tgz)

These links will take you into the "filefinder" where you will want to
enter the ship/ directory to download them.

## USE AND INSTALLATION

As manpages the documents can often be integrated with editors or
IDEs. This is a powerful tool when inspecting code that uses unfamiliar
procedures and to verify correct usage when creating code in a CLI
environment.

The process of installing manpages can vary. Check your system
documentation, but at a minimum if you create the directory $HOME/man/man3
and place the manpages there and add $HOME/man to your $MANPATH you
should be able to use the manpages. For example:
```bash
        mkdir -p $HOME/man/man3
        cd $HOME/man/man3
        tar xvfz $WHERE_YOU_PUT_TARFILE/manpages.tgz
        export MANPATH=$HOME/man:$MANPATH
        man -s 3fortran -k .
```
## DOCUMENTATION

manpage source is maintained as flat-text files which are

* run thru txt2man(1) to create the *roff file as a manpage
* and then thru manserver(1) or groff (typically) to create an additional
  HTML file as collected in
   - a simple [index of intrinsics](https://urbanjost.github.io/fortran-intrinsic-manpages/index3.html)
     and [index of statements](https://urbanjost.github.io/fortran-intrinsic-manpages/index7.html)
   - These are collected into a single document using javascript:
     [BOOK_FORTRAN](https://urbanjost.github.io/fortran-intrinsic-manpages/BOOK_FORTRAN.html).

* the example programs are extracted into the test/ directory.

## BASIC VIM USAGE

The vim(1) editor will call up a man(1) page for a word
if the letter "K" is pressed over the word and the manpage directory
is in the searchpath (among other ways, append the man/ pathname to
the environment variable MANPATH -see man(1) for more information).

Because their are name collisions with the Fortran procedures and
other languages you may want to customize your .vimrc file to use
a custom script or command when editing Fortran files. Just as 
an example, try:
```bash
   vim -c 'set keywordprg=env\ MANPATH=$HOME/man\ man\ -s\ 3fortran\ -a' test.f90
```
## CONTRIBUTING

_Collaborators are welcome_.  This is a public github repository. Anyone
can contribute, and is encouraged to do so. First, obtain a copy of
the repository:
```bash
    git clone https://github.com/urbanjost/fortran-intrinsic-manpages.git
```
### MINIMAL CHANGES

At a minimum, you can cd(1) into the md/ directory and change a flat-text
markdown file found there. Verify it converts properly to a man page with
```bash
    build-manpages/scripts/txt2man -T $MYFILE
```
You can get help on the markdown rules by entering
```bash
    build-manpages/scripts/txt2man -h
```
Then send your proposed change in using
```bash
    git add $FILENAME
    git commit -m 'changed $FILENAME because it needed it'
    git push
```
A moderator can periodically then accept your change and rebuild
the distribution files and HTML documents

The repository is set up as public, so I believe anyone with a github ID
can change these files but it is unclear to me exactly how this should
work, or whether a branch is required. Looking for more information ...

### USING THE MAKE FILE

If you are familiar with git(1) and gmake(1) (and other GNU tools) and 
are in a \*nix or CygWin environment it 
would be even better if after you change your files you cd(1) into the
build-manpages/ directory and enter:
```bash
    make
    make ship
```
and check out the HTML page generated in docs/, the manpage in man/man3/
and the demo program in test/.

The makefile is set up to do other things; and is a work in progress so
this might change.

## RESPONSES

Discussion is welcome here as well as at
 - [Fortran Discourse](https://fortran-lang.discourse.group/t/fortran-intrinsic-manpages/160/)
 - [Fortran Wiki](http://fortranwiki.org)
 - [Google Fortran newsgroup](https://groups.google.com/forum/#!forum/comp.lang.fortran)

## REFERENCES
 - The [Fortran 2018 Standard](https://j3-fortran.org/doc/year/18/18-007r1.pdf)
   as described at https://j3-fortran.org/doc/year/18/18-007r1.pdf

 - Intrinsic descriptions from Jason Blevins at the
   [Fortran Wiki](http://fortranwiki.org) http://fortranwiki.org

 - [GNU gfortran intrinsic descriptions](https://gcc.gnu.org/onlinedocs/gfortran/Intrinsic-Procedures.html)
   at https://gcc.gnu.org/onlinedocs/gfortran/Intrinsic-Procedures.html
