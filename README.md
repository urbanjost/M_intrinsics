# NAME

   ## INTRINSICS_PROJECT(7f) - [FORTRAN] intrinsic manpages
   
# DESCRIPTION

This is a project to provide a standard set of man(1) pages for the
Fortran intrinsics, with a tested working example program for each
intrinsic.

The manpage source is maintained as flat-text files which are 
   
* run thru txt2man(1) to create the *roff file as a manpage 
* and then thru groff (typically) to create an additional HTML file as collected in
   - a simple [index](https://urbanjost.github.io/fortran-intrinsic-manpages/) to
     the individual pages in HTML form
   - A single page that uses javascript to combine all the HTML 
     descriptions of the manpages is at [BOOK_FORTRAN](https://urbanjost.github.io/fortran-intrinsic-manpages/BOOK_FORTRAN.html).

* the example programs are extracted into the src/ directory.

The man(1) interface is available for all *nix systems and Cygwin as
well as other platforms.

As manpages the documents can often be integrated with editors or
IDEs. This is a powerful tool when inspecting code that uses unfamiliar
procedures and to verify correct usage when creating code.

For example the vim(1) editor will call up a man(1) page for a word
if the letter "K" is pressed over the word and the manpage directory
is in the searchpath (among other ways, append the man/ pathname to
the environment variable MANPATH -see man(1) for more information).

These documents are at the state of "good enough considering the
alternative is nothing", but are still actively being completed.

# RESPONSES

Discussion is welcome here as well as at
 - [Fortran Discourse](https://fortran-lang.discourse.group/t/fortran-intrinsic-manpages/160/)
 - [Fortran Wiki](http://fortranwiki.org)
 - [Google Fortran newsgroup]( https://groups.google.com/forum/#!forum/comp.lang.fortran)

# REFERENCES
 - The [Fortran 2018 Standard] (https://j3-fortran.org/doc/year/18/18-007r1.pdf) 
   as described at https://j3-fortran.org/doc/year/18/18-007r1.pdf
 - Intrisic descriptions from Jason Blevins at the 
   [Fortran Wiki](http://fortranwiki.org) http://fortranwiki.org
 - [GNU gfortran intrinsic descriptions](https://gcc.gnu.org/onlinedocs/gfortran/Intrinsic-Procedures.html)
   at https://gcc.gnu.org/onlinedocs/gfortran/Intrinsic-Procedures.html
