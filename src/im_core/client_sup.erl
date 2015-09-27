%% ===================================================================
%% Author xiaotie
%% 2015-8-2
%% simple_one_for_one supervisor for client server
%% ===================================================================

-module(client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD, {client, {client, start_link, []}, temporary, infinity, worker, [client]}).



%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 0, 1}, [?CHILD] } }.