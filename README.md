## NOTE
[https://fortran-lang.org](https://fortran-lang.org)
now includes these documents. They may be changed using the instructions
found there. If anyone else edits the documents there, the intent is to
sync them with this site.

This may be complicated by the fact that graphics and math expressions
may be included in Kramdown markdown, so further development there might
create documentation difficult to maintain as man-pages that display
well with man(1), as the current translator does not handle the *roff
syntax for tables, graphics, and formulas; and those features do not
typically display well if at all with the man(1) command.

## NAME

   # M_intrinsics(7f) - [FORTRAN] intrinsic man-pages

## DESCRIPTION

This is a project to 

+ provide standard Fortran intrinsic for the fortran-lang.org site
+ generate a set of man-page and HTML documents from these descriptions
+ extract demo programs from each document into an fpm(1) package for easy verification
+ generate a platform-independent Fortran program for displaying the documents

Unlike many parts of a language, intrinsics can naturally be described
in discrete units using man-pages. 

The man-pages can be used on *nix systems and Cygwin as well as other
platforms; providing the documentation in CLI (Command Line Interface)
environment.

The man-pages are available as archive files:

   - [manpages.tgz](docs/manpages.zip)
   - [manpages.zip](docs/manpages.tgz)

## USE AND INSTALLATION

As man-pages the documents can often be integrated with editors or
IDEs. This is a powerful tool when inspecting code that uses unfamiliar
procedures and to verify correct usage when creating code in a CLI
environment.

The process of installing man-pages can vary. Check your system
documentation, but at a minimum if you create the directory $HOME/man/man3
and place the man-pages there and add $HOME/man to your $MANPATH you
should be able to use the man-pages. For example:
```bash
        mkdir -p $HOME/man/man3
        cd $HOME/man/man3
        tar xvfz $WHERE_YOU_PUT_TARFILE/manpages.tgz
        export MANPATH=$HOME/man:$MANPATH
        man -s 3fortran -k .
```
## DOCUMENTATION   ![docs](docs/images/docs.gif)

The documents are maintained as Kramdown markdown files to be compatible
with the fortran-lang.org site.

Using pandoc they are then converted to flat-txt files which are

* run thru txt2man(1) to create the *roff file as a manpage
* and then thru manserver(1) or groff (typically) to create an additional
  HTML file as collected in
   - a simple [index of intrinsics](https://urbanjost.github.io/M_intrinsics/index3.html)
     and [index of statements](https://urbanjost.github.io/M_intrinsics/index7.html)
   - These are collected into a single document using javascript:
     [BOOK_FORTRAN](https://urbanjost.github.io/M_intrinsics/BOOK_FORTRAN.html).

* the example programs are extracted into the example/ directory.

## BASIC VIM USAGE

The vim(1) editor will call up a man(1) page for a word
if the letter "K" is pressed over the word and the manpage directory
is in the searchpath (among other ways, append the man/ pathname to
the environment variable MANPATH -see man(1) for more information).

Because their are name collisions with the Fortran procedures and
other languages you may want to customize your .vimrc file to use
a custom script or command when editing Fortran files. Just as 
an example, create a little Fortran file called "test.f90" and try:
```bash
   vim -c 'set keywordprg=env\ MANPATH=$HOME/man\ man\ -s\ 3fortran\ -a' test.f90
```
## CONTRIBUTING

_Collaborators are welcome_. This is a public github repository. Anyone
can contribute, and is encouraged to do so. First, create a fork of this
site from github. obtain a copy of the repository:
```bash
    git clone https://github.com/YOUR_SITE/M_intrinsics.git
```
### MINIMAL CHANGES

At a minimum, you can cd(1) into the md/ directory and change the files
there. Then push the git repository back to your fork and make a pull
request. 

If you are not familiar with this method, you can just post your changes
on this site as an "Issue".

Better yet, if you install pandoc(1) and are on a *nix system, you can run
the scripts to generate all the files and verify they appear correctly as
HTML and manpages.

Verify it converts properly to a man page with
```bash
    build-manpages/scripts/txt2man -T $MYFILE
```
You can get help on the markdown rules for txt2man(1) by entering
```bash
    build-manpages/scripts/txt2man -h
```re
Then send your proposed change in using
```bash
    git add $FILENAME
    git commit -m 'changed $FILENAME because it needed it'
    git push
```
A moderator can periodically then accept your change and rebuild
the distribution files and HTML documents.

The repository is set up as public, so I believe anyone with a github ID
can change these files but it is unclear to me exactly how this should
work, or whether a branch is required. Looking for more information ...

### USING THE MAKE FILE

If you are familiar with git(1) and gmake(1) (and other GNU tools) and
are in a \*nix or CygWin environment it would be even better if after you
change your files you cd(1) into the build-manpages/ directory and enter:
```bash
    make
    make ship
```
and check out the HTML page generated in docs/, the manpage in man/man3/
and the demo program in test/.

The makefile is set up to do other things; and is a work in progress so
this might change.

## REFERENCES
 - The [Fortran 2018 Standard](https://j3-fortran.org/doc/year/18/18-007r1.pdf)
   as described at https://j3-fortran.org/doc/year/18/18-007r1.pdf

 - Intrinsic descriptions from Jason Blevins at the
   [Fortran Wiki](http://fortranwiki.org) http://fortranwiki.org

 - [GNU gfortran intrinsic descriptions](https://gcc.gnu.org/onlinedocs/gfortran/Intrinsic-Procedures.html)
   at https://gcc.gnu.org/onlinedocs/gfortran/Intrinsic-Procedures.html

 - [Kramdown markup](https://kramdown.gettalong.org/syntax.html)
 - pandoc
 - txt2man
exit
