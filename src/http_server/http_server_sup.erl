-module(http_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Mod, Args, Type),
        {Mod, {Mod, start_link, Args}, permanent, 5000, Type, [Mod]}).



%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    % {ok, { {one_for_one, 5, 10},
    %        [?CHILD(postgresql, [http], worker),
    %         ?CHILD(redis, [], worker),
    %         ?CHILD(deps_monitor, [], worker)]} }.
    {ok, { {one_for_one, 5, 10},
           [?CHILD(postgresql, [http], worker),
            ?CHILD(redis, [], worker)]} }.