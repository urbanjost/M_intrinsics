#!/bin/bash
###############################################################################
cat <<EOF
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <meta name="generator" content="HTML Tidy for HTML5 for Linux version 5.6.0" />
  <title>$NAME</title>
</head>
<body>
EOF
###############################################################################
for NAME in $*
do
cat <<EOF
  <a href="https://play.fortran-lang.org/?code=`cat $NAME|tostream`"
  target="_blank" title="Open in Fortran Playground">
  <img src="https://raw.githubusercontent.com/fortran-lang/playground/main/frontend/src/fortran-logo.png"
  alt="Fortran logo" class="align-text-bottom" height="15.5" /> `basename $NAME .f90|sed -e 's/^demo_//'`
  </a>
  <details>
    <summary>Source</summary>
  <xmp>
EOF
cat -s $NAME
cat <<\EOF
  </xmp>
  </details>
EOF
done
###############################################################################
cat <<\EOF
</body>
</html>
EOF
###############################################################################
exit
###############################################################################
