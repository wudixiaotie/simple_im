%% ===================================================================
%% Author xiaotie
%% 2015-11-19
%% middleman supervisor
%% ===================================================================

-module(middleman_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), #{id        => I,
                          start     => {I, start_link, []},
                          restart   => permanent,
                          type      => Type}).



%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10},
           [?CHILD(middleman_worker_sup, supervisor),
            ?CHILD(middleman_manager, worker),
            ?CHILD(middleman_listener, worker)]} }.