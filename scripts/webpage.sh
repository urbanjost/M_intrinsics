#!/bin/bash
# fortran-lang/webpage
#
# henilp105 Google Summer of Code’22: Port fortran-lang.org to Sphinx (#124)
#
# fork your own copy ????
#git clone https://github.com/fortran-lang/webpage
git clone https://github.com/urbanjost/webpage
cd webpage
#Setup
# Build fortran-lang.org site (Sphinx Version)
# To install the dependencies of this project
pip3 install --user -r requirements.txt
# Build the site by invoking
python3 build.py en  # en to limit to a single language subtree
# The website will be built in build/html 
#preview by starting a webserver and opening the page with a browser 
python3 -m http.server -d build/html
cygstart http://localhost:8000

exit
###################################################################################
# After adding a new entry to package index
## ??????????
#  run the github action fortran_packages before building the sphinx build.
# Activating the pre-commit hooks for Black and Pylint:
# This assumes that you already have a cloned the main branch of this repository. Steps to activate the pre-commit hooks are:
# Make sure that you have installed all the dependencies of the repository.

pip3 install --user -r requirements.txt

    Activate the pre-commit hooks:

pre-commit install
###################################################################################
# Now, the precommit hooks have been successfully been installed into your clone.
exit
####################################################################################################################################
Steps to debug/resolve issues which prevent the commit due to pre-commit hooks:

    if pylint causes the issues in commiting to the repo, and it seems mandatory to skip the pre-commit hooks use:

SKIP=pylint git commit -m"my commit"`

    if black causes the issues in commiting to the repo, and it seems mandatory to skip the pre-commit hooks use:

SKIP=black git commit -m"my commit"

Translating via weblate

Translations can be contributed via weblate.

Translation status
Update or add translations

The documentation uses the sphinx-intl utility to generate websites for multiple languages. It generates *.po files, which contain the original sentences and a placeholder for translations.

To update translations run

python3 intl.py

if you only want to update a single translation add LANGUAGES=de to the command. This command will generate the message catalog (*.pot) and update the *.po files in the locale directory of the respective translations. Then edit the *.po files, e.g. locale/de/LC_MESSAGES/index.po. In the *.po files are paragraphs like

#: ../../pages/index.md:16
msgid "Package manager and build system for Fortran"
msgstr ""

The first line describes the file and line where to find the original text.

The second line is the original text. Don't edit this line, edit the original document instead.

The third line is meant for the translation.

To continue a long string in another line, simply close the string in the current line with " and open another one in the line underneath. E.g.

msgstr "This is "
"one string"

don't forget a space between 'is' and 'one'

After adding or updating translations build the documentation as described above.

License

This project is free software: you can redistribute it and/or modify it
under the terms of the MIT license.

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an as is basis, without
warranties or conditions of any kind, either express or implied. See the
License for the specific language governing permissions and limitations
under the License.

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in this repository by you, shall be licensed as
above, without any additional terms or conditions.  About
