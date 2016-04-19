%% ===================================================================
%% Author xiaotie
%% 2016-4-16
%% session listener
%% ===================================================================

-module(session_listener).

% APIs
-export([start_link/0, init/0]).

-include("connection.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    Pid = spawn_opt(?MODULE, init, [], [link]),
    {ok, Pid}.


init() ->
    ets:new(session, [named_table,
                      public,
                      {read_concurrency, true},
                      {write_concurrency, true}]),

    true = erlang:register(?MODULE, self()),

    SessionPort = env:get(session_port),
    log:i("[Session] Start to listen port: ~p~n", [SessionPort]),
    Opts = [binary,
            {packet, 0},
            {active, false}],
    {ok, ListenSocket} = gen_tcp:listen(SessionPort, Opts),
    accept(ListenSocket).



%% ===================================================================
%% Internal functions
%% ===================================================================

accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    case inet:peername(Socket) of
        {ok, {ClientAddr, ClientPort}} ->
            log:i("[Session] Got a connect from: ~p(~p)~n", [ClientAddr, ClientPort]),

            ok = inet:setopts(Socket, [{active, once}, {packet, 0}, binary]),
            receive
                {tcp, Socket, ?READY} ->
                    case supervisor:start_child(session_worker_sup, [Socket]) of
                        {ok, Pid} ->
                            ok = gen_tcp:controlling_process(Socket, Pid),
                            ok = inet:setopts(Socket, [{active, true}, {packet, 0}, list]);
                        Error ->
                            log:e("[Session] Worker start failed: ~p~n", [Error]),
                            ok = gen_tcp:close(Socket)
                    end
            after
                1000 ->
                    log:e("[Session] The attempt to connect timeout~n")
            end;
        _ ->
            ok
    end,
    accept(ListenSocket).