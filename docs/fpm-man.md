NAME
====

fman(1f) - \[DEVELOPER\] output descriptions of Fortran intrinsics
(LICENSE:PD)

SYNOPSIS
========

fman NAME(s) \[\[-ignorecase\]\[-**-regex**
Regular\_Expression\]\]\|\[-topic\_only\]
\[-**-color**\]\[-**-demo**\]\[-lines LINES\_PER\_AGE\]

fman \[ **--help**\| **--version**\]

DESCRIPTION
===========

fman(1) prints descriptions of Fortran intrinsics as simple flat text.

The text is formatted in the txt2man(1) markdown language so one can
easily generate man-pages on ULS (Unix-Like Systems).

OPTIONS
=======

**TOPIC(s)**

:   A list of Fortran intrinsic names or the special names "toc" and
    "manual" (which generate a table of contents and the entire set of
    documents respectively). The default is "toc" and to ignore case.

****--regex**,**-e****

:   Search all output per the provided Regular Expression. Output is
    prefixed with the topic it was found in.

****--topic\_only**,**-t****

:   Only show topic names. Other switches are ignored.

****--ignorecase**,**-i****

:   Ignore case when searching for a Regular Expression.

****--demo**,**-d****

:   extract first demo program found for a topic (starting with "program
    demo\_\*" and ending with "end program demo\_\*").

****--color****

:   Use ANSI in-line escape sequences to display the text in set colors.
    Does not work with all terminal emulators or terminals. Must use the
    **-r** switch with less(1) for less(1) to display colors.

****--lines** N,**-l** N**

:   pause every N lines. In page mode commands may be entered at the
    prompt. Enter "h" to display available commands.

****--help****

:   Display this help and exit

****--version****

:   Output version information and exit

ENVIRONMENT
===========

**FMAN\_COLORS**

:   Allows specifying the strings used by the M\_attr module to select
    colors. If set, fman(1) defaults to color mode. Default is

<!-- -->

                        FMAN_COLORS="bg='<E>',fg='<w>',prg='<c>',
                        head='<y><bo>',head_='</bo>',fixed='<w>',
                        output='<y>',output_='</bo>'"

**LINES**

:   use "export LINES" from the bash shell to use the automatically
    generated value. Set to a numeric value it activates paging of the
    output.

EXAMPLES
========

Sample commands

       fman tan|less                   # display a description of tan(3f)
       fman -d verify >demo_verify.f90 # get demo program to try VERIFY(3f).
       fman                            # list table of contents
       fman toc                        # annotated table of contents
       fman manual>fortran.txt         # create a copy of all descriptions
       fman -i --regex 'character'  # look for string in the TOC ignoring case
                                    # for string. try "trigo","size","complex"

       # list the topic "scan" if found and lines containing "scan" from the entire
       # manual, prefixing the lines with the section name, while ignoring case.
       fman -e scan -i manual

       # change background to blue, page every 30 lines
       env FMAN_COLORS="bg='<B>'" fman --color --lines 30 abs

       # Interactive session is tripped when LINES is set
       export LINES # in bash(1) sense terminal size
       fman --color # bring up Table of Contents
       t verify     # load description of intrinsic topic "verify"
       t            # load short TOC (Table of Contents)
       T            # load annotated TOC (Table of Contents)
       /trig        # move forward to a line with "trig" in it
       #            # toggle on line numbers
       h            # display crib sheet of commands
