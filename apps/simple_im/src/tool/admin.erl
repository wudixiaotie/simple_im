%% ===================================================================
%% Author xiaotie
%% 2016-4-21
%% tools for simple im admin
%% ===================================================================

-module(admin).

%% API
-export([increase_session_finder_to/1,
         decrease_session_finder_to/1,
         count_session_finder/0,
         increase_session_creator_to/1,
         decrease_session_creator_to/1,
         count_session_creator/0]).



%% ===================================================================
%% API functions
%% ===================================================================

increase_session_finder_to(NewSize) ->
    case env:get(app_mode) of
        session_server ->
            SessionFinderSize = env:get(session_finder_size),
            ok = increase_session_finder_to(NewSize, SessionFinderSize),
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
            case catch decrease_session_finder_to(NewSize, SessionFinderSize) of
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
            env:get(session_finder_size)
    end.


increase_session_creator_to(NewSize) ->
    case env:get(app_mode) of
        session_server ->
            SessionCreatorSize = env:get(session_creator_size),
            ok = increase_session_creator_to(NewSize, SessionCreatorSize),
            rpc:multicall(env, set, [session_creator_size, NewSize]);
        _ ->
            log:e("[Admin] Call increase_session_creator_to in wrong node.~n"),
            error
    end.


decrease_session_creator_to(NewSize) ->
    case env:get(app_mode) of
        session_server ->
            SessionCreatorSize = env:get(session_creator_size),
            rpc:multicall(env, set, [session_creator_size, NewSize]),
            case catch decrease_session_creator_to(NewSize, SessionCreatorSize) of
                ok ->
                    ok;
                _ ->
                    SessionCreatorSizeNow = count_session_finder(),
                    rpc:multicall(env, set, [session_creator_size, SessionCreatorSizeNow])
            end;
        _ ->
            log:e("[Admin] Call decrease_session_creator_to in wrong node.~n"),
            error
    end.


count_session_creator() ->
    case env:get(app_mode) of
        session_server ->
            Result = supervisor:count_children(session_creator_sup),
            {workers, Number} = lists:keyfind(workers, 1, Result),
            Number;
        _ ->
            env:get(session_creator_size)
    end.



%% ===================================================================
%% Internal functions
%% ===================================================================

increase_session_finder_to(NewSize, Index) when NewSize > Index ->
    NewIndex = Index + 1,
    FinderName = session_finder:name(NewIndex),
    case whereis(FinderName) of
        undefined ->
            log:i("[Admin] Start session finder:~p~n", [FinderName]),
            {ok, _} = supervisor:start_child(session_finder_sup, [NewIndex]);
        _ ->
            ok
    end,
    increase_session_finder_to(NewSize, NewIndex);
increase_session_finder_to(NewSize, _) ->
    log:i("[Admin] session_finder increased to ~p.~n", [NewSize]),
    ok.


decrease_session_finder_to(NewSize, Index) when NewSize < Index ->
    FinderName = session_finder:name(Index),
    case whereis(FinderName) of
        undefined ->
            log:e("[Admin] Can not decrease session finder:~p~n", [FinderName]);
        Pid ->
            Pid ! {stop, self()},
            receive
                ok ->
                    ok = supervisor:terminate_child(session_finder_sup, Pid)
            after
                2000 ->
                    erlang:error(timeout)
            end
    end,
    decrease_session_finder_to(NewSize, Index - 1);
decrease_session_finder_to(NewSize, _) ->
    log:i("[Admin] session_finder decreased to ~p.~n", [NewSize]),
    ok.


increase_session_creator_to(NewSize, Index) when NewSize > Index ->
    NewIndex = Index + 1,
    CreatorName = session_creator:name(NewIndex),
    case whereis(CreatorName) of
        undefined ->
            log:i("[Admin] Start session creator:~p~n", [CreatorName]),
            {ok, _} = supervisor:start_child(session_creator_sup, [NewIndex]);
        _ ->
            ok
    end,
    increase_session_creator_to(NewSize, NewIndex);
increase_session_creator_to(NewSize, _) ->
    log:i("[Admin] session creator increased to ~p.~n", [NewSize]),
    ok.


decrease_session_creator_to(NewSize, Index) when NewSize < Index ->
    CreatorName = session_creator:name(Index),
    case whereis(CreatorName) of
        undefined ->
            log:e("[Admin] Can not decrease session creator:~p~n", [CreatorName]);
        Pid ->
            Pid ! {stop, self()},
            receive
                ok ->
                    ok = supervisor:terminate_child(session_creator_sup, Pid)
            after
                2000 ->
                    erlang:error(timeout)
            end
    end,
    decrease_session_creator_to(NewSize, Index - 1);
decrease_session_creator_to(NewSize, _) ->
    log:i("[Admin] session creator decreased to ~p.~n", [NewSize]),
    ok.