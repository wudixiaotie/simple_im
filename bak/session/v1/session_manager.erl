-module (session_manager).

-behaviour (gen_server).

% APIs
-export([start_link/0, get/1, register/2, unregister/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {cursor :: integer(),
                 pool_size :: integer()}).

-compile (export_all).

%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(UserId) ->
    UserNode = where_is(UserId),
    case UserNode == node() of
        true ->
            session_worker:get(UserId);
        false ->
            get_global(UserNode, UserId)
    end.

register(UserId, Pid) ->
    catch ets:insert(session, {UserId, Pid}).

unregister(UserId) ->
    catch ets:delete(session, UserId).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    ets:new(session, [named_table, public, {read_concurrency, true}, {write_concurrency, true}]),
    session_sup:start_link(),
    PoolSize = env:get(session_poolsize),
    loop_start(PoolSize),
    State = #state{cursor = 1, pool_size = PoolSize},
    {ok, State}.

handle_call(get, _From, #state{cursor = Cursor} = State) ->
    NewCursor = Cursor rem State#state.pool_size + 1,
    {reply, erlang:get(Cursor), State#state{cursor = NewCursor}};
handle_call(_Request, _From, State) ->
    {reply, nomatch, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================

loop_start(N) when N > 0 ->
    {ok, Pid} = supervisor:start_child(session_sup, []),
    erlang:put(N, Pid),
    loop_start(N - 1);
loop_start(0) ->
    ok.

where_is(UserId) ->
    node().

get_global(UserNode, UserId) ->
    case catch gen_server:call({?MODULE, UserNode}, get) of
        {'EXIT', Reason} ->
            log:e("Reason: ~p~n", [Reason]),
            offline;
        Pid ->
            case catch gen_server:call(Pid, {get, UserId}) of
                {'EXIT', Reason} ->
                    log:e("Reason: ~p~n", [Reason]),
                    offline;
                Result ->
                    Result
            end
    end.