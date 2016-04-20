%% ===================================================================
%% Author xiaotie
%% 2016-4-16
%% session server worker supervisor
%% ===================================================================

-module(session_server_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).



%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 5, 10},
           [#{id        => session_server_worker,
              start     => {session_server_worker, start_link, []},
              restart   => temporary,
              shutdown  => brutal_kill,
              type      => worker}]} }.