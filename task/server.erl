-module(server).

-export([start/0]).


start() ->
    ssl:start(),
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
    {ok, SSLSocket} = ssl:ssl_accept(ClientSocket, [{cacertfile, "priv/ssl/cowboy-ca.crt"},
                                                    {certfile, "priv/ssl/server.crt"},
                                                    {keyfile, "priv/ssl/server.key"}]),
    ssl:setopts(SSLSocket, [{active, 300}, {packet, 0}, binary]),
    loop(ListenSocket, SSLSocket).



loop(ListenSocket, SSLSocket) ->
    receive
        Any ->
            io:format("=============~p~n", [Any])
    end,
    loop(ListenSocket, SSLSocket).