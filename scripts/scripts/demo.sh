#!/bin/bash
#@(#) extract demo program from specially formatted man pages
TOPIC="$1"
#man -s $SECTION $TOPIC|
man $TOPIC|
   col -b|
   expand|
   sed -n -e '\%^[ !]*program  *demo_%,\%^[ !]*end  *program  *demo_%{p}' 
