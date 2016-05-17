%% ===================================================================
%% Author xiaotie
%% 2015-09-14
%% postgresql client supervisor
%% ===================================================================

-module(postgresql_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD, #{id       => postgresql_worker,
                 start    => {postgresql_worker, start_link, []},
                 restart  => permanent,
                 type     => worker}).



%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 10, 5}, [?CHILD] } }.