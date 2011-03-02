#!/bin/bash
#
# @(#)update.sh
# @author Karl Ljungkvist <karl.ljungkvist.0193@student.uu.se>


LOCALBRANCH=`git branch | grep local | sed 's/.* \([a-z0-9]*-local\)/\1/'`

git stash && git checkout master && git pull && git checkout $LOCALBRANCH && git rebase master && git stash pop