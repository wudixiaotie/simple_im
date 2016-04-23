%% ===================================================================
%% Author xiaotie
%% 2016-4-16
%% session client
%% ===================================================================

-module(session).

% APIs
-export([register/2, unregister/1, find/1]).



%% ===================================================================
%% APIs
%% ===================================================================

register(UserId, Pid) ->
    Creator = get_creator(),
    case catch Creator ! {register, self(), UserId, Pid} of
        {'EXIT', _} ->
            ErrorMsg = "Session register failed",
            log:e("[IM] ~p~n", [ErrorMsg]),
            erlang:error(ErrorMsg);
        _ ->
            receive
                {session, Result} ->
                    Result
            end
    end.


unregister(undefined) ->
    ok;
unregister(UserId) ->
    Creator = get_creator(),
    case catch Creator ! {unregister, self(), UserId} of
        {'EXIT', _} ->
            ErrorMsg = "Session unregister failed",
            log:e("[IM] ~p~n", [ErrorMsg]),
            erlang:error(ErrorMsg);
        _ ->
            receive
                {session, Result} ->
                    Result
            end
    end.


find(UserId) ->
    FinderSize = env:get(session_finder_size),
    {ok, Index} = utility:random_number(FinderSize),
    FinderName = session_finder:name(Index),
    SessionServerNode = env:get(session_server_node),
    Finder = {FinderName, SessionServerNode},

    case catch Finder ! {find, self(), UserId} of
        {'EXIT', Reason} ->
            log:e("[IM] Session find failed~n", [Reason]),
            offline;
        _ ->
            receive
                {session, Result} ->
                    Result
            end
    end.



%% ===================================================================
%% Internal functions
%% ===================================================================

get_creator() ->
    CreatorSize = env:get(session_creator_size),
    {ok, Index} = utility:random_number(CreatorSize),
    CreatorName = session_creator:name(Index),
    SessionServerNode = env:get(session_server_node),
    {CreatorName, SessionServerNode}.