%% ===================================================================
%% Author xiaotie
%% 2015-11-18
%% listener for message queue
%% ===================================================================

-module(mq_listener).

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
    DefaultMQPort = env:get(mq_port),
    log:i("MQ server start listen port: ~p~n", [DefaultMQPort]),
    Opts = [binary,
            {packet, 0},
            {active, false}],
    {ok, MQListenSocket} = gen_tcp:listen(DefaultMQPort, Opts),
    accept(MQListenSocket).


accept(MQListenSocket) ->
    {ok, Socket} = gen_tcp:accept(MQListenSocket),
    ChildSpec = #{id        => erlang:port_to_list(Socket),
                  start     => {mq_worker, start_link, [Socket]},
                  restart   => temporary,
                  shutdown  => brutal_kill,
                  type      => worker},
    supervisor:start_child(mq_sup, ChildSpec),
    accept(MQListenSocket).