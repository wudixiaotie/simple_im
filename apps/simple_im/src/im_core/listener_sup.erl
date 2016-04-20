%% ===================================================================
%% Author xiaotie
%% 2015-11-25
%% simple_one_for_one supervisor for listener
%% ===================================================================

-module(listener_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD, #{id         => listener,
                 start      => {listener, start_link, []},
                 restart    => permanent,
                 type       => worker}).



%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    ssl:start(),
    join_im_network(),
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    ListenerPoolsize = env:get(im_listener_poolsize),
    ok = start_listener(ListenerPoolsize),
    {ok, Pid}.



%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 10, 5}, [?CHILD] } }.



%%====================================================================
%% Internal functions
%%====================================================================

join_im_network() ->
    InitialNode = env:get(initial_node),
    case net_adm:ping(InitialNode) of
        pong ->
            log:i("[IM] Succeed to connect to the initial node~n");
        _ ->
            ErrorMsg = "Failed to connect to the initial node",
            log:e("[IM] ~p~n", [ErrorMsg]),
            erlang:error(ErrorMsg)
    end.


start_listener(0) ->
    ok;
start_listener(N) ->
    {ok, _} = supervisor:start_child(listener_sup, [N]),
    start_listener(N - 1).