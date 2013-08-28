#!/bin/bash
#
# @(#)update.sh
# @author Karl Ljungkvist <karl.ljungkvist.0193@student.uu.se>


LOCALBRANCH="$(hostname)-local"

git stash && git checkout master && git pull && git checkout $LOCALBRANCH && git rebase master && git stash pop
