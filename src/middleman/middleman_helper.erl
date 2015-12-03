%% ===================================================================
%% Author xiaotie
%% 2015-12-03
%% middleman helper
%% ===================================================================

-module(middleman_helper).

-export([hunter_name/1, start_worker/2]).



%% ===================================================================
%% API functions
%% ===================================================================

hunter_name(Index) ->
    HunterName = erlang:list_to_atom("hunter_" ++ erlang:integer_to_list(Index)),
    {ok, HunterName}.


start_worker(Socket, hunter) ->
    middleman_hunter:start_link(Socket);
start_worker(Socket, master) ->
    middleman_master:start_link(Socket).