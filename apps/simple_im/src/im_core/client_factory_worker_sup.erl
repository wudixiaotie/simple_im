%% ===================================================================
%% Author xiaotie
%% 2015-09-26
%% client factory supervisor
%% ===================================================================

-module(client_factory_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD, #{id         => client_factory_worker,
                 start      => {client_factory_worker, start_link, []},
                 restart    => permanent,
                 type       => worker}).



%% ===================================================================
%% API functions
%% ===================================================================

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []).



%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 10, 5}, [?CHILD] } }.