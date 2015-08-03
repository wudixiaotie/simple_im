-module (log).

-export ([i/1, i/2, d/1, d/2, e/1, e/2]).

i(Format) ->
    io:format(Format).

i(Format, Args) ->
    io:format(Format, Args).

d(Format) ->
    io:format(Format).

d(Format, Args) ->
    io:format(Format, Args).

e(Format) ->
    io:format(Format).

e(Format, Args) ->
    io:format(Format, Args).