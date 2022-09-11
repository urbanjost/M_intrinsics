#!/bin/bash
# to update the webpage site go to 
#    https://github.com/fortran-lang/webpage
# and click on [fork] and create a fork under your ID
#    https://github.com/urbanjost/webpage
# on your machine go to $HOME/github/FORK and clone your fork
#    git clone https://github.com/urbanjost/webpage.git

# where my fork is
cd $HOME/github/FORK/webpage

# copy my markdown to the webpage markdown
cp $HOME/github/M_intrinsics/md/*.md ./source/learn/intrinsics/_pages/
# rebuild the site by invoking
python3 build.py en  # en to limit to a single language subtree
# The website will be built in build/html 
#preview by starting a webserver and opening the page with a browser 
python3 -m http.server -d build/html
#xdg-open http://localhost:8000
xdg-open http://localhost:8000/en/learn/intrinsics/
git status
git diff ./source/learn/intrinsics/_pages/*
cat <<\EOF
TO BE DETERMINED:

If you added a new page you probably had to change

   ./source/learn/intrinsics/*/index.md ./data/redirects.yml ./data/learning.yml

EOF
ls -ld ./source/learn/intrinsics/*/index.md 
exit

   $BROWSER firefox mozilla netscape konqueror /usr/bin/cygstart lynx w3m www-browser \
   iceweasel seamonkey iceape chromium google-chrome opera elinks links dillo
  cygstart $name    # CygWin
  start $name       # MSWindows
  xdg-open $name    # Linux
  gnome-open $name  # GNOME
  open $name        # darwin
  konquerer
  nautilus
