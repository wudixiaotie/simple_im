%% ===================================================================
%% Author xiaotie
%% 2015-11-19
%% middleman worker
%% ===================================================================

-module(middleman_worker).

-behaviour(gen_server).

% APIs
-export([start_link/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {id, socket, worke_for}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(Socket, WorkFor) ->
    gen_server:start_link(?MODULE, [Socket, WorkFor], []).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Socket, hunter]) ->
    {ok, Id} = register_hunter(),
    ok = inet:setopts(Socket, [{active, true}, {packet, 0}, binary]),
    {ok, #state{id = Id, socket = Socket, worke_for = hunter}};
init([Socket, master]) ->
    ok = inet:setopts(Socket, [{active, true}, {packet, 0}, binary]),
    {ok, #state{socket = Socket, worke_for = master}}.


handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.


handle_info({tcp, _Socket, Bin}, #state{worke_for = master} = State) ->
    Max = ets:lookup_element(hunter_list, max, 2),
    {ok, HunterId} = utility:random_number(Max),
    HunterPid = ets:lookup_element(hunter_list, HunterId, 2),
    HunterPid ! {job, Bin},
    {noreply, State};
handle_info({job, Bin}, #state{worke_for = hunter} = State) ->
    ok = gen_tcp:send(State#state.socket, Bin),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};
handle_info(_Info, State) -> {noreply, State}.


terminate(Reason, #state{id = Id} = State) ->
    case State#state.worke_for of
        hunter ->
            true = ets:delete(hunter_list, Id);
        master ->
            ok
    end,
    log:e("[Middleman] Middleman worker ~p down! Reason: ~p~n", [Id, Reason]),
    ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

register_hunter() ->
    register_hunter(1).
register_hunter(N) ->
    case ets:lookup(hunter_list, N) of
        [] ->
            true = ets:insert(hunter_list, {N, self()}),
            true = ets:insert(hunter_list, {max, N}),
            {ok, N};
        _ ->
            register_hunter(N + 1)
    end.