%% ===================================================================
%% Author xiaotie
%% 2015-11-19
%% agent for middleman
%% ===================================================================

-module(agent).

-behaviour(gen_msg).

% APIs
-export([work_for_master/0, work_for_hunter/0, offer_a_reward/1]).

% gen_msg callbacks
-export([init/1, handle_msg/2, terminate/2]).

-record(state, {socket, role}).

-include("connection.hrl").
-include("message.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

work_for_master() ->
    gen_msg:start_link({local, ?MODULE}, ?MODULE, [master], []).


work_for_hunter() ->
    gen_msg:start_link({local, ?MODULE}, ?MODULE, [hunter], []).


offer_a_reward(TomlBin) ->
    ?MODULE ! {offer_a_reward, TomlBin},
    ok.



%% ===================================================================
%% gen_msg callbacks
%% ===================================================================

init([Role]) ->
    MiddlemanHost = env:get(middleman_host),
    MiddlemanPort = env:get(middleman_port),
    {ok, Socket} = gen_tcp:connect(MiddlemanHost, MiddlemanPort, [{active, true}, {packet, 0}, binary]),
    ok = gen_tcp:send(Socket, erlang:atom_to_list(Role)),
    receive
        {tcp, Socket, ?READY} ->
            log:i("[IM] Succeed to connect to the middleman server!~n"),
            {ok, #state{socket = Socket, role = Role}}
    after
        1000 ->
            log:e("[IM] Failed to connect to the middleman server!~n"),
            ok = gen_tcp:close(Socket),
            {stop, connect_failed}
    end.


% master agent send message to middleman
handle_msg({offer_a_reward, TomlBin}, #state{role = master} = State) ->
    ok = gen_tcp:send(State#state.socket, TomlBin),
    {ok, State};
% hunter agent got message from middleman
handle_msg({tcp, _Socket, TomlBin}, #state{role = hunter} = State) ->
    {ok, TomlList} = toml:binary_2_term(TomlBin),
    ok = process_toml(TomlList),
    {ok, State};
handle_msg({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};
handle_msg(_Info, State) -> {ok, State}.


terminate(Reason, State) ->
    log:e("[IM] Middleman client is down! Reason: ~p~n", [Reason]),
    ok = gen_tcp:closed(State#state.socket),
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================

process_toml([{_, Attrs} = Toml|T]) ->
    {<<"from">>, FromId} = lists:keyfind(<<"from">>, 1, Attrs),
    ok = router:route_to_single_user(FromId, {middleman, Toml}),
    process_toml(T);
process_toml([]) ->
    ok.