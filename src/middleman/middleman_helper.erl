%% ===================================================================
%% Author xiaotie
%% 2015-12-03
%% middleman helper
%% ===================================================================

-module(middleman_helper).

-export([hunter_name/1]).



%% ===================================================================
%% API functions
%% ===================================================================

hunter_name(Index) ->
    HunterName = erlang:list_to_atom("hunter_" ++ erlang:integer_to_list(Index)),
    {ok, HunterName}.