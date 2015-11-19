%% ===================================================================
%% Author xiaotie
%% 2015-7-29
%% log print
%% ===================================================================

-module(log).

-export([i/1, i/2, d/1, d/2, e/1, e/2]).



%% ===================================================================
%% API functions
%% ===================================================================

i(Format) ->
    io:format("[info] " ++ Format).

i(Format, Args) ->
    io:format("[info] " ++ Format, Args).

d(Format) ->
    io:format("[debug] " ++ Format).

d(Format, Args) ->
    io:format("[debug] " ++ Format, Args).

e(Format) ->
    io:format("\e[1;31m[error] " ++ Format ++ "\e[0;38m").

e(Format, Args) ->
    io:format("\e[1;31m[error] " ++ Format ++ "\e[0;38m", Args).