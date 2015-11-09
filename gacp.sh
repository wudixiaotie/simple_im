#!/bin/sh

if [ -n "$1" ] ;then
  note=$1
else
  note="update"
fi

rm -rf erl_crash.dump
git add -A
git commit -m "$note"
git push origin master