%% ===================================================================
%% Author xiaotie
%% 2016-4-21
%% tools for simple im admin
%% ===================================================================

-module(admin).

%% API
-export([increase_session_finder/1, decrease_session_finder/1,
         count_session_finder/0]).



%% ===================================================================
%% API functions
%% ===================================================================

increase_session_finder(N) ->
    case env:get(app_mode) of
        im ->
            SessionFinderSize = env:get(session_finder_size),
            ok = increase_session_finder(N, SessionFinderSize + 1);
        _ ->
            log:e("[Admin] Call increase_session_finder in wrong node.~n"),
            error
    end.


decrease_session_finder(N) ->
    case env:get(app_mode) of
        im ->
            SessionFinderSize = env:get(session_finder_size),
            ok = env:set(session_finder_size, SessionFinderSize - N),
            case catch decrease_session_finder(N, SessionFinderSize) of
                ok ->
                    ok;
                _ ->
                    SessionFinderSizeNow = count_session_finder(),
                    env:set(session_finder_size, SessionFinderSizeNow)
            end;
        _ ->
            log:e("[Admin] Call decrease_session_finder in wrong node.~n"),
            error
    end.


count_session_finder() ->
    Result = supervisor:count_children(session_finder_worker_sup),
    {workers, Number} = lists:keyfind(workers, 1, Result),
    Number.



%% ===================================================================
%% Internal functions
%% ===================================================================

increase_session_finder(0, Index) ->
    NewSessionFinderSize = Index - 1,
    ok = env:set(session_finder_size, NewSessionFinderSize),
    log:i("[Admin] session_finder increased to ~p.~n", [NewSessionFinderSize]),
    ok;
increase_session_finder(N, Index) ->
    FinderName = session_finder:name(Index),
    case whereis(FinderName) of
        undefined ->
            log:i("[Admin] Start session finder:~p~n", [FinderName]),
            {ok, _} = supervisor:start_child(session_finder_worker_sup, [Index]);
        _ ->
            ok
    end,
    increase_session_finder(N - 1, Index + 1).


decrease_session_finder(0, NewSessionFinderSize) ->
    log:i("[Admin] session finder decreased to ~p.~n", [NewSessionFinderSize]),
    ok;
decrease_session_finder(N, Index) ->
    FinderName = session_finder:name(Index),
    case whereis(FinderName) of
        undefined ->
            log:e("[Admin] Can not decrease session finder:~p~n", [FinderName]);
        Pid ->
            Pid ! {stop, self()},
            receive
                ok ->
                    ok = supervisor:terminate_child(session_finder_worker_sup, Pid)
            after
                2000 ->
                    erlang:error(timeout)
            end
    end,
    decrease_session_finder(N - 1, Index - 1).