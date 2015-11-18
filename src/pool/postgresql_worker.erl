%% ===================================================================
%% Author xiaotie
%% 2015-9-12
%% postgresql client pool worker
%% ===================================================================

-module(postgresql_worker).

-export([start_link/5]).



%% ===================================================================
%% API functions
%% ===================================================================

start_link(DbHost, DbUsername, DbPassword, Opts, Index) ->
    {ok, Conn} = epgsql:connect(DbHost, DbUsername, DbPassword, Opts),
    ets:insert(postgresql_connection, {Index, Conn}),    
    {ok, Conn}.