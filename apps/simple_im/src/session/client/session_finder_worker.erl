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
        {find, From, UserIdBin} ->
            ok = gen_tcp:send(Socket, <<$f, UserIdBin/binary>>),

            Result = receive
                {tcp, _Socket, <<"offline">>} ->
                    offline;
                {tcp, _Socket, PidBin} ->
                    Pid = erlang:binary_to_term(PidBin),
                    {ok, Pid}
            after
                1000 ->
                    log:e("[IM] find session timeout~n"),
                    {error, timeout}
            end,

            From ! {session, Result}
    end,
    loop(Socket).