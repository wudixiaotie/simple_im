-module (tgen_server2).

-behaviour (gen_server).

% APIs
-export([start_link/0, t/0, worker/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {cursor :: integer(),
                 pool_size :: integer()}).


%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

t() ->
    io:format("~p~n", [os:timestamp()]),
    t(100000).
t(N) when N > 0 ->
    ?MODULE ! t,
    t(N - 1);
t(0) ->
    ?MODULE ! t1,
    ok.

worker() ->
    receive
        _ ->
            worker()
    end.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    ets:new(session, [named_table]),
    loop(10),
    {ok, #state{cursor = 1, pool_size = 10}}.

handle_call(_Request, _From, State) ->
    {reply, nomatch, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% {update, insert, {UserId, Pid}} | {update, delete, UserId}
handle_info(t, #state{cursor = Cursor} = State) ->
    NewCursor = Cursor rem State#state.pool_size + 1,
    worker_name(Cursor) ! t,
    {noreply, State#state{cursor = NewCursor}};
handle_info(_Info, State) ->
    io:format("~p~n", [os:timestamp()]),
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================
loop(N) when N > 0 ->
    Pid = spawn(fun ?MODULE:worker/0),
    erlang:register(worker_name(N), Pid),
    loop(N - 1);
loop(0) ->
    ok.

worker_name(Index) ->
    IndexStr = erlang:integer_to_list(Index),
    erlang:list_to_atom("worker_" ++ IndexStr).