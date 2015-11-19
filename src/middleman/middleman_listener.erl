%% ===================================================================
%% Author xiaotie
%% 2015-11-19
%% middleman listener
%% ===================================================================

-module(middleman_listener).

% APIs
-export([start_link/0]).



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
    DefaultMQPort = env:get(middleman_port),
    log:i("Middleman server start listen port: ~p~n", [DefaultMQPort]),
    Opts = [binary,
            {packet, 0},
            {active, false}],
    {ok, MQListenSocket} = gen_tcp:listen(DefaultMQPort, Opts),
    accept(MQListenSocket).


accept(MQListenSocket) ->
    {ok, Socket} = gen_tcp:accept(MQListenSocket),
    {ok, Pid} = supervisor:start_child(middleman_worker_sup, [Socket]),
    ok = gen_tcp:controlling_process(Socket, Pid),
    accept(MQListenSocket).