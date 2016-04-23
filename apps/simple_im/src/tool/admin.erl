%% ===================================================================
%% Author xiaotie
%% 2016-4-21
%% tools for simple im admin
%% ===================================================================

-module(admin).

%% API
% -export([increase_session_finder_to/1,
%          decrease_session_finder_to/1,
%          count_session_finder/0,
%          increase_session_creator_to/1,
%          decrease_session_creator_to/1,
%          count_session_creator/0]).
-export([increase_session_finder_to/1,
         decrease_session_finder_to/1,
         count_session_finder/0]).



%% ===================================================================
%% API functions
%% ===================================================================

increase_session_finder_to(NewSize) ->
    case env:get(app_mode) of
        session_server ->
            SessionFinderSize = env:get(session_finder_size),
            ok = increase_session_worker_to(session_finder_sup, NewSize, SessionFinderSize),
            rpc:multicall(env, set, [session_finder_size, NewSize]);
        _ ->
            log:e("[Admin] Call increase_session_finder_to in wrong node.~n"),
            error
    end.


decrease_session_finder_to(NewSize) ->
    case env:get(app_mode) of
        session_server ->
            SessionFinderSize = env:get(session_finder_size),
            rpc:multicall(env, set, [session_finder_size, NewSize]),
            case catch decrease_session_worker_to(session_finder_sup, NewSize, SessionFinderSize) of
                ok ->
                    ok;
                _ ->
                    SessionFinderSizeNow = count_session_finder(),
                    rpc:multicall(env, set, [session_finder_size, SessionFinderSizeNow])
            end;
        _ ->
            log:e("[Admin] Call decrease_session_finder_to in wrong node.~n"),
            error
    end.


count_session_finder() ->
    case env:get(app_mode) of
        session_server ->
            Result = supervisor:count_children(session_finder_sup),
            {workers, Number} = lists:keyfind(workers, 1, Result),
            Number;
        _ ->
            log:e("[Admin] Call count_session_finder in wrong node.~n"),
            error
    end.



%% ===================================================================
%% Internal functions
%% ===================================================================

increase_session_worker_to(WorkerSup, NewSize, Index) when NewSize > Index ->
    NewIndex = Index + 1,
    FinderName = session_finder:name(NewIndex),
    case whereis(FinderName) of
        undefined ->
            log:i("[Admin] Start session finder:~p~n", [FinderName]),
            {ok, _} = supervisor:start_child(WorkerSup, [NewIndex]);
        _ ->
            ok
    end,
    increase_session_worker_to(WorkerSup, NewSize, NewIndex);
increase_session_worker_to(WorkerSup, NewSize, _) ->
    log:i("[Admin] ~p's child increased to ~p.~n", [WorkerSup, NewSize]),
    ok.


decrease_session_worker_to(WorkerSup, NewSize, Index) when NewSize < Index ->
    FinderName = session_finder:name(Index),
    case whereis(FinderName) of
        undefined ->
            log:e("[Admin] Can not decrease session finder:~p~n", [FinderName]);
        Pid ->
            Pid ! {stop, self()},
            receive
                ok ->
                    ok = supervisor:terminate_child(WorkerSup, Pid)
            after
                2000 ->
                    erlang:error(timeout)
            end
    end,
    decrease_session_worker_to(WorkerSup, NewSize, Index - 1);
decrease_session_worker_to(WorkerSup, NewSize, _) ->
    log:i("[Admin] ~p's child decreased to ~p.~n", [WorkerSup, NewSize]),
    ok.