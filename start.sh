#!/bin/sh
rebar compile

erl_start="erl -pa ebin/ -pa deps/*/ebin/ -smp +Q 1100000 +P 1100000"

# ip="192.168.1.137"
ip="192.168.3.5"

case $1 in
    "o" )
        erl_command="$erl_start -eval \"observer:start().\""
        ;;
    "a" )
        erl_command="$erl_start -eval \"application:start(simple_im).\""
        ;;
    "oa" | "ao" )
        erl_command="$erl_start -eval \"observer:start().\" -eval \"application:start(simple_im).\""
        ;;
    "n1" )
        erl_command="$erl_start -name s1@$ip"
        ;;
    "n2" )
        erl_command="$erl_start -name s2@$ip -eval \"net_adm:ping('s1@$ip').\""
        ;;
    "n3" )
        erl_command="$erl_start -name s3@$ip -eval \"net_adm:ping('s1@$ip').\""
        ;;
    "" )
        erl_command="$erl_start"
        ;;
    * )
        echo "unknown args!"
        ;;
esac

case $2 in
    "o" )
        $erl_command -eval "observer:start()."
        ;;
    "a" )
        $erl_command -eval "application:start(simple_im)."
        ;;
    "oa" | "ao" )
        $erl_command -eval "observer:start()." -eval "application:start(simple_im)."
        ;;
    "n1" )
        $erl_command -name s1@$ip
        ;;
    "n2" )
        $erl_command -name s2@$ip -eval "net_adm:ping('s1@$ip')."
        ;;
    "" )
        $erl_command
        ;;
    * )
        echo "unknown args!"
        ;;
esac