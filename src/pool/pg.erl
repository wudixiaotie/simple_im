%% ===================================================================
%% Author xiaotie
%% 2015-9-12
%% postgresql client pool manager
%% ===================================================================
-module (pg).

-behaviour(gen_server).

% APIs
-export([start_link/0, query/1, query/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {cursor :: integer(), pool_size :: integer()}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

query(QueryStr) ->
    query(QueryStr, []).

query(QueryStr, Parameters) ->
    {ok, Cursor} = gen_server:call(?MODULE, get_cursor),
    case catch ets:lookup_element(pg_conn, Cursor, 2) of
        {'EXIT', _} ->
            {error, get_conn_failed};
        Conn ->
            epgsql:equery(Conn, QueryStr, Parameters)
    end.



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    ets:new(pg_conn, [named_table, public, {read_concurrency, true}]),
    pg_sup:start_link(),
    DbPoolSize = env:get(db_poolsize),
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
    supervisor:start_child(pg_sup, [N]),
    create_connection(N - 1).