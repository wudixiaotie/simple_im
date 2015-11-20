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

init([Socket, WorkFor]) ->
    {ok, Id} = utility:random_binary_16(),
    ok = inet:setopts(Socket, [{active, true}, {packet, 0}, binary]),
    true = ets:insert(hunter_list, {Id, self()}),
    {ok, #state{id = Id, socket = Socket, worke_for = WorkFor}}.


handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.


handle_info({tcp, Socket, _Data}, #state{worke_for = master} = State) ->
    ok = gen_tcp:send(Socket, <<"ok">>),
    {noreply, State};
handle_info({tcp_closed, _Socket}, #state{worke_for = master} = State) ->
    {stop, tcp_closed, State};
handle_info(_Info, State) -> {noreply, State}.


terminate(Reason, State) ->
    case State#state.worke_for of
        hunter ->
            true = ets:delete(hunter_list, State#state.id);
        master ->
            ok
    end,
    log:e("[Middleman] Worker ~p down! Reason: ~p~n", [State#state.socket, Reason]),
    ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================