%% ===================================================================
%% Author xiaotie
%% 2016-05-05
%% middleman http worker supervisor
%% ===================================================================

-module(middleman_http_worker_sup).

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
           [#{id        => middleman_http_worker,
              start     => {middleman_http_worker, start_link, []},
              restart   => temporary,
              shutdown  => brutal_kill,
              type      => worker}]} }.