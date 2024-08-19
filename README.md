The short version is to take the standalone/fman.f90 file, compile it
and you have a CLI (Command Line Interface) to descriptions of the
Fortran intrinsics; with instructions available via "fman --help".

The descriptions can be previewed as
[HTML](https://urbanjost.github.io/M_intrinsics/index3.html)

If you want man-pages, HTML, or markdown descriptions; a Fortran module
you can call with your own programs to display the descriptions, or to
integrate the descriptions into fpm(1) as a plugin, read on.

## Name

   # M_intrinsics(7f) - [FORTRAN] intrinsic man-pages

## Description

Unlike many parts of the Fortran language, intrinsics can naturally be
described in discrete units using man-page-like descriptions.

This is a project to 

+ provide standard Fortran intrinsic markdown descriptions for the fortran-lang.org site 
+ generate a set of man-page and HTML Fortran intrinsic documents
+ extract [demo programs](https://urbanjost.github.io/M_intrinsics/playground.html)
  from each document into an fpm(1) package for easy verification
+ generate a platform-independent Fortran program for displaying the documents

## Man-pages and Standalone Source

The man-pages can be used on *nix systems and Cygwin as well as other
platforms; providing a convenient CLI (Command Line Interface).

The man-pages are available as archive files:

<!--
   - [manpages.tgz](docs/manpages.zip)
   - [manpages.zip](docs/manpages.tgz)
   -->
   - [manpages.tgz](https://urbanjost.github.io/M_intrinsics/manpages.tgz)
   - [manpages.zip](https://urbanjost.github.io/M_intrinsics/manpages.zip)

A single-file version of the CLI program fpm-man(1) is in 
[fman.f90](https://raw.githubusercontent.com/urbanjost/M_intrinsics/master/standalone/fman.f90).

   - builds with `gfortran  fman.f90`
   - builds with `ifort -O1 fman.f90`

## Project Information

### Use and Installation

The man-pages can often be integrated with editors or IDEs. This is a
powerful reference when inspecting code that uses unfamiliar procedures
and to verify correct usage when creating code in a CLI environment.

The process of installing man-pages can vary. Check your system
documentation, but at a minimum if you create the directory $HOME/man/man3
and place the man-pages there and add $HOME/man to your $MANPATH you
should be able to use the man-pages. For example:
```bash
        mkdir -p $HOME/man/man3
        cd $HOME/man/man3
        tar xvfz $WHERE_YOU_PUT_TARFILE/manpages.tgz
	cd ..
        mandb -c .
        export MANPATH=$HOME/man:$MANPATH
	export MANWIDTH=80   # optional, displays manpages in columns 1 to 80
        man -s 3fortran -k . # list topic line for all Fortran man-pages
```

Typical installation on a Linux platform as an administrator ( but it varies) :
```bash
     # as the administrator
     cd /usr/share/man/man3
     tar xvfz /$WHERE_YOU_PUT_TARFILE/manpages.tgz
     cd /usr/share/man
     mandb -c .
```
     then anyone on that platform can enter commands like 
```bash
     man sinh.3fortran     # specifically show Fortran sinh(3) documentation
     man sinh.3f           # section abbreviations are often supported for this format
     man -k . -s 3fortran  # list all fortran pages
     man -s 3fortran --regex '.*' |col -b # show all Fortran intrinsics
```
    See man(1) (ie. enter "man man") for more information.
    
## Documentation Formats  ![docs](docs/images/docs.gif)

The documents are maintained as Myst markdown files to be compatible
with the fortran-lang.org site.

Using pandoc(1) they are then converted to flat-text files which are

* run thru txt2man(1) to create the *roff file as a manpage
* and then thru manserver(1) or groff (typically) to create an additional
  HTML file as collected in
   - a simple [index of intrinsics](https://urbanjost.github.io/M_intrinsics/index3.html)
     and [index of statements](https://urbanjost.github.io/M_intrinsics/index7.html)
   - These are collected into a single document using javascript:
     [BOOK_FORTRAN](https://urbanjost.github.io/M_intrinsics/BOOK_FORTRAN.html).
   - All pages in a single [slidy HTML file](https://urbanjost.github.io/M_intrinsics/intrinsics_slidy.html)
   - [documentation shortcuts](https://urbanjost.github.io/M_intrinsics)

* the example programs are extracted into the [example/](example/) directory.

## Basic VIM Usage

The vim(1) editor will call up a man(1) page for a word
if the letter "K" is pressed over the word and the manpage directory
is in the searchpath (among other ways, append the man/ pathname to
the environment variable MANPATH -see man(1) for more information).

Because there are name collisions with the Fortran procedures and
other languages you may want to customize your .vimrc file to use
a custom script or command when editing Fortran files. Just as 
an example, create a little Fortran file called "test.f90" and try:
```bash
   vim -c 'set keywordprg=env\ MANPATH=$HOME/man\ man\ -s\ 3fortran\ -a' test.f90
```
## Contributing

_Collaborators are welcome_. This is a public github repository. Anyone
can contribute, and is encouraged to do so. First, create a fork of this
site from github. obtain a copy of the repository:
```bash
    git clone https://github.com/YOUR_SITE/M_intrinsics.git
```
### Minimal Changes

At a minimum, you can cd(1) into the md/ directory and change the files
there. Then push the git repository back to your fork and make a pull
request. 

If you are not familiar with this method, you can just post your changes
on this site as an "Issue".

A moderator can periodically then accept your change and rebuild
the distribution files and HTML documents.

The repository is set up as public, so I believe anyone with a github ID
can change these files but it is unclear to me exactly how this should
work, or whether a branch is required. Looking for more information ...

### Using the Make File

You have to have a Fortran compiler, fpm(1) and pandoc(1) installed to
use the method used here to convert the markdown files to man-pages,
a Fortran utility program, and HTML. For reference
``` 
# use pandoc to convert the markdown files to text
scripts/scripts/totxt.sh
# run make(1) to generate all the other files
cd scripts
make
make ship
```
and check out the HTML page generated in docs/, the manpages in man/man3/
and the demo programs in example

## References
 - The [Fortran 2018 Standard](https://j3-fortran.org/doc/year/18/18-007r1.pdf)
   as described at https://j3-fortran.org/doc/year/18/18-007r1.pdf

 - Intrinsic descriptions from Jason Blevins at the
   [Fortran Wiki](http://fortranwiki.org) http://fortranwiki.org

 - [GNU gfortran intrinsic descriptions](https://gcc.gnu.org/onlinedocs/gfortran/Intrinsic-Procedures.html)
   at https://gcc.gnu.org/onlinedocs/gfortran/Intrinsic-Procedures.html

 - [Myst-Markdown](https://myst-parser.readthedocs.io/en/latest/)
 - [Kramdown markup](https://kramdown.gettalong.org/syntax.html)
 - [pandoc](https://pandoc.org/)
 - [txt2man](https://github.com/mvertes/txt2man)

## Footnotes
[https://fortran-lang.org/en/learn/intrinsics](https://fortran-lang.org)
now includes an earlier version of these documents. They
may be changed using the instructions found in the github
[webpage](https://github.com/fortran-lang/webpage) repository.

Keeping these original documents synced with those may be complicated by
the fact that graphics, Unicode  and math expressions may be included
in the descriptions in the future, so further development there might
create documentation difficult to maintain as man-pages that display
well with man(1) (as the current translator does not handle the \*roff
syntax for tables, graphics, and formulas); and those features do not
typically display well if at all with the man(1) command.

This page allows you to generate the CLI (Command Line Interface)
command fpm-man (which is often renamed to fman(1)) that allows the
fpm command to display the documents with "fpm man TOPIC" and provides
an archive file with the intrinsic manpages on *nix machines (ie. on
machines with the man(1) command) so it will continue to be maintained,
but the differences between these and the on-line fortran-lang.org pages
may become more extensive as time goes on.

## Todo

Additional descriptions of procedures from the standard modules

Descriptions of most statements (READ, WRITE, EXIT, ...) in addition to
the intrinsics.

<!--
Converting from the primary source in txt/ in txt2man(1) format and
generating the markdown to the Myst markdown in md/ being used to
generate the txt/ directory with pandoc made sense as the primary
target became fortran-lang.org documents instead of man-pages but the
pandoc(1) conversion to /txt and then manpages leaves the plain text
without proper indentation. Other conversion paths like going straight
to manpages and generating text from them for the CLI interface are
being explored to provide better formatting.
-->

Add

 + RANDOM_INIT
 + COSHAPE
 + FAILED_IMAGES
 + GET_TEAM
 + IMAGE_STATUS
 + LCOBOUND
 + STOPPED_IMAGES
 + TEAM_NUMBER
 + UCOBOUND
