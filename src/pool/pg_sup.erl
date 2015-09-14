-module (pg_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Args), {pg_worker, {pg_worker, start_link, Args}, permanent, brutal_kill, worker, [pg_worker]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    DbHost = env:get(db_host),
    DbUsername = env:get(db_username),
    DbPassword = env:get(db_password),
    DbDatabase = env:get(db_database),
    ConnSpec = ?CHILD([DbHost, DbUsername, DbPassword, [
        {database, DbDatabase},
        {timeout, 4000}
    ]]),

    {ok, { {simple_one_for_one, 10, 5}, [ConnSpec] } }.