%% ===================================================================
%% Author xiaotie
%% 2016-4-23
%% im
%% ===================================================================

-module(im).

% APIs
-export([start/0]).



%% ===================================================================
%% APIs
%% ===================================================================

start() ->
    ssl:start(),

    ListenerPoolsize = env:get(im_listener_poolsize),
    ok = start_listener(ListenerPoolsize),

    ok = ping_nodes(),
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================

start_listener(0) ->
    ok;
start_listener(N) ->
    {ok, _} = supervisor:start_child(listener_sup, [N]),
    start_listener(N - 1).


ping_nodes() ->
    InitialNode = env:get(initial_node),
    case net_adm:ping(InitialNode) of
        pong ->
            log:i("[IM] Succeed to connect to the initial node~n");
        _ ->
            ErrorMsg1 = "Failed to connect to the initial node",
            log:e("[IM] ~p~n", [ErrorMsg1]),
            erlang:error(ErrorMsg1)
    end,

    SessionServerNode = env:get(session_server_node),
    case net_adm:ping(SessionServerNode) of
        pong ->
            {im_node_monitor, SessionServerNode} ! {nodeinit, node()},
            log:i("[IM] Succeed to connect to the session server node~n");
        _ ->
            ErrorMsg2 = "Failed to connect to the session server node",
            log:e("[IM] ~p~n", [ErrorMsg2]),
            erlang:error(ErrorMsg2)
    end,

    SessionFinderSize = rpc:call(SessionServerNode, env, get, [session_finder_size]),
    env:set(session_finder_size, SessionFinderSize),
    SessionCreatorSize = rpc:call(SessionServerNode, env, get, [session_creator_size]),
    env:set(session_creator_size, SessionCreatorSize),
    ok.