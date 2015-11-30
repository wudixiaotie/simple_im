%% ===================================================================
%% Author xiaotie
%% 2015-8-2
%% listener for simpile im
%% ===================================================================

-module(listener).

-behaviour(gen_server).

% APIs
-export([start_link/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {listen_socket, cf_name, acceptor_ref}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(Index) ->
    Name = erlang:list_to_atom("listener_" ++ erlang:integer_to_list(Index)),
    gen_server:start_link({local, Name}, ?MODULE, [Index], []).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Index]) ->
    {ok, CfName} = cf:start_link(Index),

    DefaultIMPort = env:get(im_port),
    {ok, Port} = utility:free_port(DefaultIMPort),
    log:i("[IM] Server start to listen port: ~p~n", [Port]),
    Opts = [binary,
            {packet, 0},
            {reuseaddr, true},
            {keepalive, true},
            {backlog, 30000},
            {active, false}],
    {ok, ListenSocket} = gen_tcp:listen(Port, Opts),
    {ok, AcceptorRef} = prim_inet:async_accept(ListenSocket, -1),
    State = #state{listen_socket = ListenSocket,
                   cf_name = CfName,
                   acceptor_ref = AcceptorRef},
    
    {ok, [{IP, _, _}, _]} = inet:getif(),
    {ok, IMListKey} = redis:key(im_list),
    redis:q([<<"HSET">>, IMListKey, utility:ip_port(IP, Port), 1]),
    {ok, State}.


handle_call(_Msg, _From, State) -> {reply, _Msg, State}.
handle_cast(_Msg, State) -> {noreply, State}.


handle_info({inet_async, ListenSocket, AcceptorRef, {ok, ClientSocket}},
             #state{listen_socket = ListenSocket, acceptor_ref = AcceptorRef} = State) ->
    %% Taken from prim_inet.  We are merely copying some socket options from the
    %% listening socket to the new TCP socket.
    true = inet_db:register_socket(ClientSocket, inet_tcp),
    {ok, Opts} = prim_inet:getopts(ListenSocket, [active, nodelay, keepalive, delay_send, priority, tos]),
    ok = prim_inet:setopts(ClientSocket, Opts),
    case {inet:sockname(ClientSocket), inet:peername(ClientSocket)} of
        {{ok, {ServerAddr, ServerPort}}, {ok, {ClientAddr, ClientPort}}} ->
            log:i("[IM] Listener accept socket:(~w), server:~w(~p), client:~w(~p)~n",
                  [ClientSocket, ServerAddr, ServerPort, ClientAddr, ClientPort]),
            cf:make(State#state.cf_name, ClientSocket);
        _ ->
            ok
    end,
    {ok, NewAcceptorRef} = prim_inet:async_accept(ListenSocket, -1),
    {noreply, State#state{acceptor_ref = NewAcceptorRef}};
handle_info(Info, State) ->
    log:e("[IM] Listener got unknown request:~p~n", [Info]),
    {noreply, State}.


terminate(_Reason, State) ->
    ok = gen_tcp:close(State#state.listen_socket),
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================