%% ===================================================================
%% Author xiaotie
%% 2015-11-19
%% agent for middleman
%% ===================================================================

-module(agent).

-behaviour(gen_server).

% APIs
-export([work_for_master/0, work_for_hunter/0, offer_a_reward/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {socket, role}).

-include("connection.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

work_for_master() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [master], []).


work_for_hunter() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [hunter], []).


offer_a_reward(TomlBin) ->
    ok = gen_server:call(?MODULE, {offer_a_reward, TomlBin}),
    ok.



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Role]) ->
    MiddlemanHost = env:get(middleman_host),
    MiddlemanPort = env:get(middleman_port),
    {ok, Socket} = gen_tcp:connect(MiddlemanHost, MiddlemanPort, [{active, true}, {packet, 0}, binary]),
    ok = gen_tcp:send(Socket, erlang:atom_to_list(Role)),
    receive
        {tcp, Socket, ?READY} ->
            log:i("[Middleman] Agent init success!~n"),
            {ok, #state{socket = Socket, role = Role}}
    after
        1000 ->
            log:e("[Middleman] Agent init failed!~n"),
            ok = gen_tcp:close(Socket),
            {stop, connect_failed}
    end.


% master agent send message to middleman
handle_call({offer_a_reward, TomlBin}, _From, #state{role = master} = State) ->
    ok = gen_tcp:send(State#state.socket, TomlBin),
    {reply, ok, State};
handle_call(_Request, _From, State) -> {reply, nomatch, State}.


handle_cast(_Msg, State) -> {noreply, State}.


% hunter agent got message from middleman
handle_info({tcp, _Socket, TomlBin}, #state{role = hunter} = State) ->
    % {ok, Toml} = toml:binary_2_term(TomlBin),
    % {<<"to">>, To}
    io:format("=Agent got: ~p~n", [TomlBin]),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};
handle_info(_Info, State) -> {noreply, State}.


terminate(Reason, _State) ->
    log:e("[Middleman] Agent is down! Reason: ~p~n", [Reason]),
    ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================