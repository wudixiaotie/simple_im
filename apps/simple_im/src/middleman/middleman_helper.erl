%% ===================================================================
%% Author xiaotie
%% 2015-12-03
%% middleman helper
%% ===================================================================

-module(middleman_helper).

-export([im_worker_name/1, http_worker_name/1]).



%% ===================================================================
%% API functions
%% ===================================================================

im_worker_name(Index) ->
    IMWorkerName = erlang:list_to_atom("im_worker_" ++ erlang:integer_to_list(Index)),
    {ok, IMWorkerName}.


http_worker_name(Index) ->
    HttpWorkerName = erlang:list_to_atom("http_worker_" ++ erlang:integer_to_list(Index)),
    {ok, HttpWorkerName}.