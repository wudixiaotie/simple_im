#!/bin/sh
rebar compile

erl_start="erl -pa ebin/ -pa deps/*/ebin/ -smp +Q 1100000 +P 1100000"

ip="192.168.3.5"

case $1 in
    "o" )
        $erl_start -eval "observer:start ()."
        ;;
    "a" )
        $erl_start -eval "application:start(simple_im)."
        ;;
    "n1" )
        $erl_start -name s1@$ip
        ;;
    "n2" )
        $erl_start -name s2@$ip -eval "net_adm:ping('s1@$ip')."
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