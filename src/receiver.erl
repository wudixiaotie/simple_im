-module (receiver).

-behaviour(gen_server).

% APIs
-export([start_link/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record(state, {socket,
                heartbeat_timeout}).

%% ===================================================================
%% APIs
%% ===================================================================

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Socket]) ->
    State = #state{socket = Socket,
                   heartbeat_timeout = env:get(heartbeat_timeout)},
    setopts(State#state.socket),
    {ok, State, State#state.heartbeat_timeout}.
handle_call(_Request, _From, State) ->
    {reply, nomatch, State, State#state.heartbeat_timeout}.
handle_cast(_Msg, State) ->
    {noreply, State, State#state.heartbeat_timeout}.


handle_info({tcp, Socket, Data}, #state{socket = Socket} = State) ->
    io:format ("===Got msg: ~p~n", [Data]),
    {ok, Toml} = etoml:parse(Data),
    case Toml of
        [{<<"msg">>, Attrs}] ->
            {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
            Ack = <<"[ack] id=\"", MsgId/binary, "\"">>,
            io:format ("===Send ack: ~p~n", [Ack]),
            gen_tcp:send(Socket, Ack);
        [{<<"ack">>, Attrs}] ->
            {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
            io:format ("===Msg id=~p send success~n", [MsgId])
    end,
    {noreply, State, State#state.heartbeat_timeout};
% tcp connection change to passive
handle_info({tcp_passive, Swocket}, #state{socket = Swocket} = State) ->
    setopts(Swocket),
    {noreply, State, State#state.heartbeat_timeout};
% connection closed
handle_info({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};
handle_info(timeout, State) ->
    proc_lib:hibernate(gen_server, enter_loop, [?MODULE, [], State]),
    {noreply, State, State#state.heartbeat_timeout};
handle_info(Info, State) ->
    io:format("Request received non_tcp: ~p.~n", [Info]),
    {noreply, State, State#state.heartbeat_timeout}.


terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================

setopts(Swocket) ->
    inet:setopts(Swocket, [{active, 300}, {packet, 0}, binary]).