#!/bin/bash
#
# mklinks.sh: Make links from one directory to another if
# distinct.  Create target.
#
# Args:
# 1: target stamp file
# 2: source directory
# 3-: all files to be linked
#
# $Id: mklinks.sh 792 2015-04-17 14:07:44Z jrobcary $
#
# Copyright &copy; 2005-2015, Tech-X Corporation, Boulder, CO.
# See LICENSE file (EclipseLicense.txt) for conditions of use.
#
######################################################################


# Get directories and files
stampfile=$1
shift
sourcedir=`(cd $1; pwd -P)`
shift
destdir=`pwd -P`
files="$*"
echo Linking $* from $sourcedir to $destdir.
if test "$destdir" = "$sourcedir"; then
  touch $stampfile
  echo "Not making links as source directory is destination directory."
  exit 0
fi
rm -f $files
for i in $files; do
  ln -s $sourcedir/$i $destdir/$i
done
touch $stampfile

