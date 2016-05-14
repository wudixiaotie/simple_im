%% ===================================================================
%% Author xiaotie
%% 2015-9-12
%% postgresql client pool worker
%% ===================================================================

-module(postgresql_worker).

-export([start_link/1]).



%% ===================================================================
%% API functions
%% ===================================================================

start_link(Index) ->
    DbHost = env:get(db_host),
    DbUsername = env:get(db_username),
    DbPassword = env:get(db_password),
    DbDatabase = env:get(db_database),
    DbPort = env:get(db_port),
    Opts = [{database, DbDatabase},
            {port, DbPort},
            {timeout, 4000}],

    {ok, Conn} = epgsql:connect(DbHost, DbUsername, DbPassword, Opts),
    ets:insert(postgresql_connection, {Index, Conn}),
    {ok, Conn}.