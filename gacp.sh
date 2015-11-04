#!/bin/sh

if [ -n "$1" ] ;then
  branch=$1
else
  branch="master"
fi

if [ -n "$2" ] ;then
  note=$2
else
  note="update"
fi

rm -rf erl_crash.dump
git checkout $branch
git add -A
git commit -m "$note"
git push -u origin $branch