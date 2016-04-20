%% ===================================================================
%% Author xiaotie
%% 2016-4-20
%% session finder worker sup
%% ===================================================================

-module(session_finder_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).



%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    ok = session_finder:start(),
    {ok, Pid}.



%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 5, 10},
           [#{id        => session_finder_worker,
              start     => {session_finder_worker, start_link, []},
              restart   => permanent,
              shutdown  => brutal_kill,
              type      => worker}]} }.