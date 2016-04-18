%% ===================================================================
%% Author xiaotie
%% 2016-4-16
%% session client
%% ===================================================================

-module(session).

-behaviour(gen_msg).

% APIs
-export ([start_link/0, register/2, unregister/1, find/1]).

% gen_msg callbacks
-export([init/1, handle_msg/2, terminate/2]).

-record(state, {socket}).

-include("connection.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    gen_msg:start_link({local, ?MODULE}, ?MODULE, [], []).


register(UserId, Pid) ->
    UserIdBin = erlang:integer_to_binary(UserId),
    PidBin = erlang:term_to_binary(Pid),
    ?MODULE ! {register, self(), UserIdBin, PidBin},
    receive
        {session, Result} ->
            Result
    end.


unregister(undefined) ->
    ok;
unregister(UserId) ->
    UserIdBin = erlang:integer_to_binary(UserId),
    ?MODULE ! {unregister, self(), UserIdBin},
    receive
        {session, Result} ->
            Result
    end.


find(UserId) ->
    UserIdBin = erlang:integer_to_binary(UserId),
    ?MODULE ! {find, self(), UserIdBin},
    receive
        {session, Result} ->
            Result
    end.



%% ===================================================================
%% gen_msg callbacks
%% ===================================================================

init([]) ->
    InitialNode = env:get(initial_node),
    case net_adm:ping(InitialNode) of
        pong ->
            log:i("[IM] Succeed to connect to the initial node~n");
        _ ->
            log:e("[IM] Failed to connect to the initial node~n")
    end,

    SessionHost = env:get(session_host),
    SessionPort = env:get(session_port),
    {ok, Socket} = gen_tcp:connect(SessionHost, SessionPort, [{active, true}, {packet, 0}, binary]),
    gen_tcp:send(Socket, ?READY),
    log:i("[IM] Succeed to connect to the session server~n"),
    {ok, #state{socket = Socket}}.


handle_msg({register, From, UserIdBin, PidBin}, State) ->
    ok = gen_tcp:send(State#state.socket, <<$r, UserIdBin/binary, $:, PidBin/binary>>),

    Result = receive
        {tcp, _Socket, ?OK} ->
            ok;
        {tcp, _Socket, OldPidBin} ->
            OldPid = erlang:binary_to_term(OldPidBin),
            Pid = erlang:binary_to_term(PidBin),
            OldPid ! {replaced_by, Pid},
            ok
    after
        1000 ->
            log:e("[IM] register session timeout~n"),
            {error, timeout}
    end,

    From ! {session, Result},
    {ok, State};
handle_msg({unregister, From, UserIdBin}, State) ->
    ok = gen_tcp:send(State#state.socket, <<$u, UserIdBin/binary>>),

    Result = receive
        {tcp, _Socket, ?OK} ->
            ok
    after
        1000 ->
            log:e("[IM] unregister session timeout~n"),
            {error, timeout}
    end,

    From ! {session, Result},
    {ok, State};
handle_msg({find, From, UserIdBin}, State) ->
    ok = gen_tcp:send(State#state.socket, <<$f, UserIdBin/binary>>),

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

    From ! {session, Result},
    {ok, State};
handle_msg({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};
handle_msg(_Info, State) -> {ok, State}.


terminate(Reason, _State) ->
    log:e("[IM] session terminate:~p~n", [Reason]),
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================