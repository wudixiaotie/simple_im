%% ===================================================================
%% Author xiaotie
%% 2015-11-19
%% middleman listener
%% ===================================================================

-module(middleman_listener).

% APIs
-export([start_link/0, init/0]).

-include("connection.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    Pid = erlang:spawn_opt(?MODULE, init, [], [link]),
    {ok, Pid}.


init() ->
    true = erlang:register(?MODULE, self()),
    MiddlemanPort = env:get(middleman_port),
    log:i("[Middleman] Start to listen port: ~p~n", [MiddlemanPort]),
    Opts = [binary,
            {packet, 0},
            {active, false}],
    {ok, MiddlemanListenSocket} = gen_tcp:listen(MiddlemanPort, Opts),
    accept(MiddlemanListenSocket).



%% ===================================================================
%% Internal functions
%% ===================================================================

accept(MiddlemanListenSocket) ->
    {ok, Socket} = gen_tcp:accept(MiddlemanListenSocket),
    case inet:peername(Socket) of
        {ok, {ClientAddr, ClientPort}} ->
            log:i("[Middleman] Got a connect from: ~p(~p)~n", [ClientAddr, ClientPort]),

            ok = inet:setopts(Socket, [{active, once}, {packet, 0}, binary]),
            receive
                {tcp, Socket, AppModeBin} ->
                    ok = gen_tcp:send(Socket, ?READY),
                    Supervisor = case AppModeBin of
                        <<"http">> ->
                            middleman_http_worker_sup;
                        <<"im">> ->
                            middleman_im_worker_sup
                    end,
                    case supervisor:start_child(Supervisor, [Socket]) of
                        {ok, Pid} ->
                            ok = gen_tcp:controlling_process(Socket, Pid),
                            ok = inet:setopts(Socket, [{active, true}, {packet, 0}, binary]);
                        Error ->
                            log:e("[Middleman] Worker start failed: ~p~n", [Error]),
                            ok = gen_tcp:close(Socket)
                    end
            after
                1000 ->
                    log:e("[Middleman] The attempt to connect timeout~n")
            end;
        _ ->
            ok
    end,
    accept(MiddlemanListenSocket).