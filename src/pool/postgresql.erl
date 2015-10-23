%% ===================================================================
%% Author xiaotie
%% 2015-9-12
%% postgresql client pool manager
%% ===================================================================

-module (postgresql).

-behaviour(gen_server).

% APIs
-export([start_link/0, start_link/1, exec/1, exec/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {cursor :: integer(), pool_size :: integer()}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [im], []).

start_link(http) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [http], []);
start_link(_) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [im], []).


exec(Sql) ->
    exec(Sql, []).

exec(Sql, Parameters) ->
    {ok, Cursor} = gen_server:call(?MODULE, get_cursor),
    case catch ets:lookup_element(postgresql_connection, Cursor, 2) of
        {'EXIT', _} ->
            {error, get_conn_failed};
        Conn ->
            apply(epgsql, equery, [Conn, Sql, Parameters])
    end.



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Mode]) ->
    ets:new(postgresql_connection, [named_table, public, {read_concurrency, true}]),
    postgresql_sup:start_link(),
    DbPoolSize = case Mode of
        http ->
            env:get(http_db_poolsize);
        _ ->
            env:get(db_poolsize)
    end,
    ok = create_connection(DbPoolSize),
    {ok, #state{cursor = 1, pool_size = DbPoolSize}}.


handle_call(get_cursor, _From, #state{cursor = Cursor} = State) ->
    case Cursor + 1 > State#state.pool_size of
        true ->
            NewCursor = 1;
        false ->
            NewCursor = Cursor + 1
    end,
    {reply, {ok, Cursor}, State#state{cursor = NewCursor}};
handle_call(_Request, _From, State) ->
    {reply, nomatch, State}.


handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================

create_connection(0) ->
    ok;
create_connection(N) ->
    supervisor:start_child(postgresql_sup, [N]),
    create_connection(N - 1).