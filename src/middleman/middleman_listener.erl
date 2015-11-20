%% ===================================================================
%% Author xiaotie
%% 2015-11-19
%% middleman listener
%% ===================================================================

-module(middleman_listener).

% APIs
-export([start_link/0]).

-include("connection.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    Pid = erlang:spawn_link(fun() -> init() end),
    {ok, Pid}.



%% ===================================================================
%% Internal functions
%% ===================================================================

init() ->
    true = erlang:register(?MODULE, self()),
    MiddlemanPort = env:get(middleman_port),
    ets:new(hunter_list, [named_table, public, {read_concurrency, true}]),
    log:i("[Middleman] Start to listen port: ~p~n", [MiddlemanPort]),
    Opts = [binary,
            {packet, 0},
            {active, false}],
    {ok, MiddlemanListenSocket} = gen_tcp:listen(MiddlemanPort, Opts),
    accept(MiddlemanListenSocket).


accept(MiddlemanListenSocket) ->
    {ok, Socket} = gen_tcp:accept(MiddlemanListenSocket),
    case inet:peername(Socket) of
        {ok, {ClientAddr, ClientPort}} ->
            log:i("[Middleman] Got a connect from: ~p(~p)~n", [ClientAddr, ClientPort]),

            ok = inet:setopts(Socket, [{active, once}, {packet, 0}, binary]),
            receive
                {tcp, Socket, Role} ->
                    ok = gen_tcp:send(Socket, ?READY),
                    WorkFor = erlang:binary_to_atom(Role),
                    case supervisor:start_child(middleman_worker_sup, [Socket, WorkFor]) of
                        {ok, Pid} ->
                            ok = gen_tcp:controlling_process(Socket, Pid);
                        _ ->
                            log:e("[Middleman] worker start failed~n"),
                            ok = gen_tcp:close(Socket)
                    end
            after
                1000 ->
                    log:e("[Middleman] worker start timeout~n")
            end;
        _ ->
            ok
    end,
    accept(MiddlemanListenSocket).