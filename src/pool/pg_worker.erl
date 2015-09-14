-module (pg_worker).

-export ([start_link/5]).


start_link(DbHost, DbUsername, DbPassword, Opts, Index) ->
    {ok, Conn} = epgsql:connect(DbHost, DbUsername, DbPassword, Opts),
    ets:insert(pg_conn, {Index, Conn}),    
    {ok, Conn}.