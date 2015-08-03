#!/bin/sh
rebar compile

erl_start="erl -pa ebin/ -pa deps/*/ebin/ -smp +Q 1100000 +P 1100000"

case $1 in
    "o" )
        $erl_start -eval "observer:start ()."
        ;;
    "a" )
        $erl_start -eval "application:start(simple_im)."
        ;;
    "n1" )
        $erl_start -name s1@192.168.1.137
        ;;
    "n2" )
        $erl_start -name s2@192.168.1.137
        ;;
    "oa" | "ao" )
        $erl_start -eval "observer:start ()." -eval "application:start(simple_im)."
        ;;
    "" )
        $erl_start
        ;;
    * )
        echo "unknown args!"
        ;;
esac