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
    SessionFile = env:get(session_file),
    {ok, TableRef} = case filelib:is_file(SessionFile) of
        true ->
            dets:open_file(SessionFile);
        false ->
            dets:open_file(session, [{access, read_write},
                                     {auto_save, 60000}, % 1 minute
                                     {file, "/tmp/session.dets"},
                                     {keypos, 1},
                                     {ram_file, true}])
    end,

    true = erlang:register(?MODULE, self()),

    SessionPort = env:get(session_port),
    log:i("[Session] Start to listen port: ~p~n", [SessionPort]),
    Opts = [binary,
            {packet, 0},
            {active, false}],
    {ok, ListenSocket} = gen_tcp:listen(SessionPort, Opts),
    accept(ListenSocket, TableRef).



%% ===================================================================
%% Internal functions
%% ===================================================================

accept(ListenSocket, TableRef) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    case inet:peername(Socket) of
        {ok, {ClientAddr, ClientPort}} ->
            log:i("[Session] Got a connect from: ~p(~p)~n", [ClientAddr, ClientPort]),

            ok = inet:setopts(Socket, [{active, once}, {packet, 0}, binary]),
            receive
                {tcp, Socket, ?READY} ->
                    case supervisor:start_child(session_worker_sup, [Socket, TableRef]) of
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
    accept(ListenSocket, TableRef).