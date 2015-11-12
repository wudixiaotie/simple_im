%% ===================================================================
%% Author xiaotie
%% 2015-9-26
%% Client factory:
%% Two type of client connect: first login, should verify the token
%% and create a client for it, or replace one if its exists.
%% second reconnect, should find the original gen_server then connect
%% to it.
%% ===================================================================

-module (cf).

-behaviour(gen_server).

% APIs
-export([start_link/0, make/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {queue, max :: integer()}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


make(Socket) ->
    {ok, WorkerName} = gen_server:call(?MODULE, get_worker),
    case whereis(WorkerName) of
        undefined ->
            error;
        WorkerPid ->
            WorkerPid ! {make, Socket},
            gen_tcp:controlling_process(Socket, WorkerPid),
            ok
    end.


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    cf_sup:start_link(),
    ConnectSize = env:get(connect_size),
    ok = create_multiple_worker(ConnectSize),
    State = #state{queue = queue:new(), max = ConnectSize},
    {ok, State}.


handle_call(get_worker, _From, State) ->
    NewState = case queue:out(State#state.queue) of
        {empty, _} ->
            NewMax = State#state.max + 1,
            {ok, WorkerName} = create_single_worker(NewMax),
            State#state{max = NewMax};
        {{value, WorkerName}, NewQueue} ->
            State#state{queue = NewQueue}
    end,
    {reply, {ok, WorkerName}, NewState};
handle_call(_Request, _From, State) -> {reply, nomatch, State}.



handle_cast(_Msg, State) -> {noreply, State}.


handle_info({free_worker, Name}, State) ->
    NewQueue = queue:in(Name, State#state.queue),
    {noreply, State#state{queue = NewQueue}};
handle_info(Info, State) ->
    log:e("cf got unknown request:~p~n", [Info]),
    {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================

create_multiple_worker(0) ->
    ok;
create_multiple_worker(N) ->
    create_single_worker(N),
    create_multiple_worker(N - 1).


create_single_worker(Index) ->
    Name = worker_name(Index),
    supervisor:start_child(cf_sup, [Name]),
    {ok, Name}.


worker_name(Index) ->
    erlang:list_to_atom("cf_worker_" ++ erlang:integer_to_list(Index)).