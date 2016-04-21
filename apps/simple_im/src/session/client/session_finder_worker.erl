%% ===================================================================
%% Author xiaotie
%% 2016-4-20
%% session finder worker
%% ===================================================================

-module(session_finder_worker).

% APIs
-export([start_link/1, init/1]).

-include("connection.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

start_link(Index) ->
    Pid = erlang:spawn_opt(?MODULE, init, [Index], [link]),
    {ok, Pid}.


init(Index) ->
    Name = session_finder:name(Index),
    true = erlang:register(Name, self()),

    SessionHost = env:get(session_host),
    SessionPort = env:get(session_port),
    {ok, Socket} = gen_tcp:connect(SessionHost, SessionPort, [{active, true}, {packet, 0}, binary]),
    gen_tcp:send(Socket, ?READY),
    log:i("[IM] Session finder succeed to connect to the session server~n"),
    loop(Socket).



%% ===================================================================
%% Internal functions
%% ===================================================================

loop(Socket) ->
    receive
        Message -> 
            do_loop(Message, Socket)
    end,
    loop(Socket).


do_loop({find, From, UserIdBin}, Socket) ->
    ok = gen_tcp:send(Socket, <<$f, UserIdBin/binary>>),

    receive
        {tcp, _Socket, <<"offline">>} ->
            From ! {session, offline};
        {tcp, _Socket, PidBin} ->
            Pid = erlang:binary_to_term(PidBin),
            From ! {session, {ok, Pid}};
        {tcp_closed, _Socket} ->
            From ! {session, offline},
            erlang:error(tcp_closed)
    after
        1000 ->
            log:e("[IM] find session timeout~n"),
            From ! {session, {error, timeout}}
    end;
do_loop(stop, Socket) ->
    ok = gen_tcp:close(Socket),
    log:e("[IM] Session finder terminate~n"),
    erlang:exit(normal);
do_loop(_, _) ->
    log:e("[IM] Session finder got unexpected message~n").