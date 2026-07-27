###################################################################################
# After adding a new entry to package index
## ??????????
#  run the github action fortran_packages before building the sphinx build.
# Activating the pre-commit hooks for Black and Pylint:
# This assumes that you already have a cloned the main branch of this repository. Steps to activate the pre-commit hooks are:
# Make sure that you have installed all the dependencies of the repository.
cd $HOME/github/FORK
cd webpage
pip3 install --user -r requirements.txt
#    Activate the pre-commit hooks:
pre-commit install
# Now, the precommit hooks have been successfully been installed into your clone.
exit
