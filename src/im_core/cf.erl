%% ===================================================================
%% Author xiaotie
%% 2015-9-26
%% Client factory:
%% Two type of client connect: first login, should verify the token
%% and create a client for it, or replace one if its exists.
%% second reconnect, should find the original gen_server then connect
%% to it.
%% ===================================================================

-module(cf).

-behaviour(gen_server).

% APIs
-export([start_link/1, make/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {name :: atom(),
                cf_worker_sup_name :: atom(),
                queue,
                max :: integer()}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(Index) ->
    Name = erlang:list_to_atom("cf_" ++ erlang:integer_to_list(Index)),
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).


make(CFPid, Socket) ->
    {ok, WorkerName} = gen_server:call(CFPid, get_worker),
    case whereis(WorkerName) of
        undefined ->
            error;
        WorkerPid ->
            WorkerPid ! {make, Socket},
            ok = gen_tcp:controlling_process(Socket, WorkerPid),
            ok
    end.


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Name]) ->
    CFWorkerSupName = erlang:list_to_atom(erlang:atom_to_list(Name) ++ "_cf_worker_sup"),
    cf_worker_sup:start_link(CFWorkerSupName),
    CFSize = env:get(cf_size),
    ok = create_multiple_worker(Name, CFWorkerSupName, CFSize),
    State = #state{name = Name,
                   cf_worker_sup_name = CFWorkerSupName,
                   queue = queue:new(),
                   max = CFSize},
    {ok, State}.


handle_call(get_worker, _From, State) ->
    NewState = case queue:out(State#state.queue) of
        {empty, _} ->
            NewMax = State#state.max + 1,
            {ok, WorkerName} = create_single_worker(State#state.name,
                                                    State#state.cf_worker_sup_name,
                                                    NewMax),
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
    log:e("[IM] Client factory got unknown request:~p~n", [Info]),
    {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================

create_multiple_worker(_, _, 0) ->
    ok;
create_multiple_worker(CFName, CFWorkerSupName, N) ->
    create_single_worker(CFName, CFWorkerSupName, N),
    create_multiple_worker(CFName, CFWorkerSupName, N - 1).


create_single_worker(CFName, CFWorkerSupName, WorkerIndex) ->
    Name = worker_name(CFName, WorkerIndex),
    supervisor:start_child(CFWorkerSupName, [Name, CFName]),
    {ok, Name}.


worker_name(CFName, WorkerIndex) ->
    erlang:list_to_atom(erlang:atom_to_list(CFName) ++
                        "_cf_worker_" ++
                        erlang:integer_to_list(WorkerIndex)).