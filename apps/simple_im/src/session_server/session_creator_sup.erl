%% ===================================================================
%% Author xiaotie
%% 2016-4-23
%% session server creator supervisor
%% ===================================================================

-module(session_creator_sup).

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
           [#{id        => session_creator,
              start     => {session_creator, start_link, []},
              restart   => permanent,
              shutdown  => brutal_kill,
              type      => worker}]} }.