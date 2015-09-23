#!/bin/sh
rebar compile

erl_start="erl -pa ebin/ -pa deps/*/ebin/ -smp +Q 1100000 +P 1100000"

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
        erl_command="$erl_start -name s1@simple_im.com"
        ;;
    "n2" )
        erl_command="$erl_start -name s2@simple_im.com"
        ;;
    "n3" )
        erl_command="$erl_start -name s3@simple_im.com"
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
        $erl_command -name s1@simple_im.com
        ;;
    "n2" )
        $erl_command -name s2@simple_im.com 
        ;;
    "" )
        $erl_command
        ;;
    * )
        echo "unknown args!"
        ;;
esac