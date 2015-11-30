-module(server).

-export([start/0]).


start() ->
    Opts = [binary,
            {packet, 0},
            {reuseaddr, true},
            {keepalive, true},
            {backlog, 30000},
            {active, false}],
    {ok, ListenSocket} = gen_tcp:listen(9999, Opts),
    accept(ListenSocket).


accept(ListenSocket) ->
    {ok, AcceptorRef} = prim_inet:async_accept(ListenSocket, -1),
    receive
        {inet_async, ListenSocket, AcceptorRef, {ok, ClientSocket}} ->
            ok
    end,
    true = inet_db:register_socket(ClientSocket, inet_tcp),
    {ok, Opts1} = prim_inet:getopts(ListenSocket, [active, nodelay, keepalive, delay_send, priority, tos]),
    ok = prim_inet:setopts(ClientSocket, Opts1),
    loop(ListenSocket, ClientSocket).



loop(ListenSocket, Socket) ->
io:format("=============1~n"),
    case gen_tcp:recv(Socket, 0) of
        {error,ebadf} ->
            accept(ListenSocket);
        Data ->
            io:format("=============~p~n", [Data])
    end,
    loop(ListenSocket, Socket).