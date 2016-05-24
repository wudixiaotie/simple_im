%% ===================================================================
%% Author xiaotie
%% 2015-08-02
%% listener for simpile im
%% ===================================================================

-module(listener).

-behaviour(gen_msg).

% APIs
-export([start_link/1]).

% gen_msg callbacks
-export([init/1, handle_msg/2, terminate/2]).

-record(state, {listen_socket, client_factory, acceptor_ref}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(Index) ->
    Name = erlang:list_to_atom("listener_" ++ erlang:integer_to_list(Index)),
    gen_msg:start_link({local, Name}, ?MODULE, [Index], []).



%% ===================================================================
%% gen_msg callbacks
%% ===================================================================

init([Index]) ->
    {ok, ClientFactory} = client_factory:start_link(Index),

    DefaultIMPort = env:get(im_port),
    {ok, Port} = utility:free_port(DefaultIMPort),
    log:i("[IM] Server with SSL/TLS encryption start to listen port: ~p~n", [Port]),
    Opts = [binary,
            {packet, 0},
            {reuseaddr, true},
            {keepalive, true},
            {backlog, 30000},
            {active, false}],
    {ok, ListenSocket} = gen_tcp:listen(Port, Opts),
    {ok, AcceptorRef} = prim_inet:async_accept(ListenSocket, -1),
    State = #state{listen_socket = ListenSocket,
                   client_factory = ClientFactory,
                   acceptor_ref = AcceptorRef},

    {ok, [{IP, _, _}, _]} = inet:getif(),
    {ok, IMListKey} = redis:key(im_list),
    redis:q([<<"HSET">>, IMListKey, utility:ip_port(IP, Port), 1]),
    {ok, State}.


handle_msg({inet_async, ListenSocket, AcceptorRef, {ok, ClientSocket}},
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
            client_factory:make(State#state.client_factory, ClientSocket);
        _ ->
            ok
    end,
    {ok, NewAcceptorRef} = prim_inet:async_accept(ListenSocket, -1),
    {ok, State#state{acceptor_ref = NewAcceptorRef}};
handle_msg(Info, State) ->
    log:e("[IM] Listener got unknown request:~p~n", [Info]),
    {ok, State}.


terminate(_Reason, State) ->
    ok = gen_tcp:close(State#state.listen_socket),
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================