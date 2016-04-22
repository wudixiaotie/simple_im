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
    UserIdBin = erlang:integer_to_binary(UserId),
    PidBin = erlang:term_to_binary(Pid),
    case catch session_registrar ! {register, self(), UserIdBin, PidBin} of
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
    UserIdBin = erlang:integer_to_binary(UserId),
    case catch session_registrar ! {unregister, self(), UserIdBin} of
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

    UserIdBin = erlang:integer_to_binary(UserId),
    case catch FinderName ! {find, self(), UserIdBin} of
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