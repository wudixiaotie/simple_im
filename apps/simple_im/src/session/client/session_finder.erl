%% ===================================================================
%% Author xiaotie
%% 2016-4-20
%% session finder worker
%% ===================================================================

-module(session_finder).

% APIs
-export([start/0, name/1]).



%% ===================================================================
%% APIs
%% ===================================================================

start() ->
    FinderSize = env:get(session_finder_size),
    ok = start_worker(FinderSize),
    ok.


name(Index) ->
    IndexStr = erlang:integer_to_list(Index),
    erlang:list_to_atom("session_finder_" ++ IndexStr).



%% ===================================================================
%% Internal functions
%% ===================================================================

start_worker(0) ->
    ok;
start_worker(N) ->
    {ok, _} = supervisor:start_child(session_finder_worker_sup, [N]),
    start_worker(N - 1).