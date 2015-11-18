%% ===================================================================
%% Author xiaotie
%% 2015-8-2
%% listener for simpile im
%% ===================================================================

-module(listener).

-behaviour(gen_server).

% APIs
-export([start_link/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {listen_socket, acceptor_ref}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    DefaultPort = env:get(port),
    {ok, Port} = utility:free_port(DefaultPort),
    log:i("IM server start listen port: ~p~n", [Port]),
    Opts = [binary,
            {packet, 0},
            {reuseaddr, true},
            {keepalive, true},
            {backlog, 30000},
            {active, false}],
    {ok, ListenSocket} = gen_tcp:listen(Port, Opts),
    {ok, AcceptorRef} = prim_inet:async_accept(ListenSocket, -1),
    State = #state{listen_socket = ListenSocket, acceptor_ref = AcceptorRef},
    
    {ok, [{IP, _, _}, _]} = inet:getif(),
    {ok, IMListKey} = redis:key(im_list),
    redis:q([<<"HSET">>, IMListKey, utility:ip_port(IP, Port), 1]),
    {ok, State}.


handle_call(_Msg, _From, State) -> {reply, _Msg, State}.
handle_cast(_Msg, State) -> {noreply, State}.


handle_info({inet_async, ListenSocket, AcceptorRef, {ok, ClientSocket}},
             #state{listen_socket = ListenSocket, acceptor_ref = AcceptorRef} = State) ->
    case set_sockopt(State#state.listen_socket, ClientSocket) of
        ok ->
            case {inet:sockname(ClientSocket), inet:peername(ClientSocket)} of
                {{ok, {ServerAddr, ServerPort}}, {ok, {ClientAddr, ClientPort}}} ->
                    log:i("listener accept socket: (~w),server:~s(~p),client:~s(~p)~n",
                          [ClientSocket, inet_parse:ntoa(ServerAddr), ServerPort,
                           inet_parse:ntoa(ClientAddr), ClientPort]),
                    cf:make(ClientSocket);
                _ ->
                    ok
            end,
            case prim_inet:async_accept(ListenSocket, -1) of
                {ok,    NewAcceptorRef} -> ok;
                {error, NewAcceptorRef} ->
                    exit({async_accept, inet:format_error(NewAcceptorRef)})
            end,
            NewState = State#state{acceptor_ref = NewAcceptorRef},
            {noreply, NewState};
        Error ->
            {stop, Error, State}
    end;
handle_info(Info, State) ->
    log:e("Listener got unknown request:~p~n", [Info]),
    {noreply, State}.


terminate(_Reason, State) ->
    ok = gen_tcp:close(State#state.listen_socket),
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

%% Taken from prim_inet.  We are merely copying some socket options from the
%% listening socket to the new TCP socket.
set_sockopt(ListenSocket, ClientSocket) ->
    true = inet_db:register_socket(ClientSocket, inet_tcp),
    case prim_inet:getopts(ListenSocket, [active, nodelay, keepalive, delay_send, priority, tos]) of
        {ok, Opts} ->
            case prim_inet:setopts(ClientSocket, Opts) of
                ok ->
                    ok;
                Error ->
                    ok = gen_tcp:close(ClientSocket),
                    Error
            end;
        Error ->
            ok = gen_tcp:close(ClientSocket), Error
    end.