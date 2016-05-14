%% ===================================================================
%% Author xiaotie
%% 2016-05-14
%% ssdb client supervisor
%% ===================================================================

-module(ssdb_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD, #{id       => ssdb_client,
                 start    => {ssdb_client, start_link, []},
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