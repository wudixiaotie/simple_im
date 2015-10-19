-module (tsup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args), {I, {I, start_link, Args}, permanent, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(N) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [N]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([1]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(tg, [])]} };
init([2]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(tg, [tsup])]} }.