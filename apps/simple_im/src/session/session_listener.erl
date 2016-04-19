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
    dets:open_file(session, [{access, read_write},
                             {auto_save, 60000}, % 1 minute
                             {file, "/tmp/session.dets"},
                             {keypos, 1},
                             {ram_file, true}]),

    ok = recover_data(),

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

recover_data() ->
    SessionFile = env:get(session_file),
    case filelib:is_file(SessionFile) of
        true ->
            {ok, OldTableRef} = dets:open_file(SessionFile),
            Start = dets:bchunk(OldTableRef, start),
            Input = init_bchunk(OldTableRef, Start),
            ok = dets:init_table(session, Input, [{format,bchunk}]);
        false ->
            ok
    end.


init_bchunk(Tab, State) ->
    fun(read) when State =:= '$end_of_table' ->
        end_of_input;
       (read) when element(1, State) =:= error ->
        State;
       (read) ->
        {Cont, Objs} = State,
        {Objs, init_bchunk(Tab, dets:bchunk(Tab, Cont))};
       (close) ->
        ok
    end.


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