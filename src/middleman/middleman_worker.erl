%% ===================================================================
%% Author xiaotie
%% 2015-11-19
%% middleman worker
%% ===================================================================

-module(middleman_worker).

-behaviour(gen_server).

% APIs
-export([start_link/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {socket, index}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Socket]) ->
    {ok, Index} = middleman_manager:register(),
    true = ets:insert(roster, {Index, self()}),
    ok = inet:setopts(Socket, [{active, true}, {packet, 0}, binary]),
    ok = gen_tcp:send(Socket, <<"connect success">>),
    {ok, #state{socket = Socket, index = Index}}.


handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.


handle_info({tcp, Socket, Data}, State) ->
    ok = gen_tcp:send(Socket, <<"ok">>),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};
handle_info(_Info, State) -> {noreply, State}.


terminate(Reason, State) ->
    middleman_manager ! unregister,
    true = ets:delete(roster, State#state.index),
    log:e("Middleman worker ~p down! Reason: ~p~n", [State#state.socket, Reason]),
    ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================