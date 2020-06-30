# NAME

   ## INTRINSICS_PROJECT(7f) - [FORTRAN] intrinsic manpages
   
# DESCRIPTION

This is a project to provide a standard set of man(1) pages for the
Fortran intrinsics, with a tested working example program for each
intrinsic.

The manpage source is maintained as flat-text files which are 
   
* run thru txt2man(1) to create the *roff file as a manpage 
* and then thru groff (typically) to create an additional HTML file in 
  [html/](https://urbanjost.github.io/fortran-intrinsics-manpages/docs/FORTRAN_BOOK.html).

* the example program is extracted into the src/ directory.

The man(1) interface is available for all *nix systems and Cygwin as
well as other platforms.

As manpages the documents can often be integrated with editors or
IDEs. This is a powerful tool when inspecting code that uses unfamiliar
procedures and to verify correct usage when creating code.

For example the vim(1) editor will call up a man(1) page for a word
if the letter "K" is pressed over the word and the manpage directory
is in the searchpath (among other ways, append the man/ pathname to
the environment variable MANPATH -see man(1) for more information).

*Note*: In many cases the descriptions of these intrinsics were
derived from the [[GFortran|GNU Fortran]] and the Fortran Wiki by
Jason Blevins. These documents were under the [[GNU Free Documentation
License]]; as are these documents currently.

These documents are at the state of "good enough considering the
alternative is nothing", but are still actively being completed.
