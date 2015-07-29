#!/bin/sh

if [ -n "$1" ] ;then
  note=$1
else
  note="update"
fi

git add -A
git commit -m "$note"
git push -u origin master
