#!/bin/sh
rebar3 compile

erl_start="erl -pa _build/default/lib/*/ebin/ -smp +Q 1100000 +P 1100000 +K true"

case $1 in
    "n1" )
        erl_command="$erl_start -name s1@simple_im.com"
        ;;
    "n2" )
        erl_command="$erl_start -name s2@simple_im.com"
        ;;
    "n3" )
        erl_command="$erl_start -name s3@simple_im.com"
        ;;
    "http" )
        erl_command="$erl_start -config config/http/sys.config -eval simple_im:start(http)."
        ;;
    "middleman" )
        erl_command="$erl_start -config config/middleman/sys.config -eval simple_im:start(middleman)."
        ;;
    "task" )
        erl_command="$erl_start -eval postgresql:start_link()."
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
        erl_command="$erl_command -eval observer:start()."
        ;;
    "im" )
        erl_command="$erl_command -config config/im/sys.config -eval simple_im:start()."
        ;;
    "" )
        erl_command=$erl_command
        ;;
    * )
        echo "unknown args!"
        ;;
esac

case $3 in
    "o" )
        $erl_command -eval "observer:start()."
        ;;
    "" )
        $erl_command
        ;;
    * )
        echo "unknown args!"
        ;;
esac