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
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    ListenerPoolsize = env:get(im_listener_poolsize),
    ok = start_listener(ListenerPoolsize),
    {ok, Pid}.



%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 10, 5}, [?CHILD] } }.


start_listener(0) ->
    ok;
start_listener(N) ->
    supervisor:start_child(listener_sup, [N]),
    start_listener(N - 1).