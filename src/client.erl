-module(client).

-behaviour(gen_server).

% APIs
-export([start_link/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record(state, {socket}).

%% ===================================================================
%% APIs
%% ===================================================================

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Socket]) ->
    State = #state{socket = Socket},
    setopts(State#state.socket),
    {ok, State}.
handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.


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
    {noreply, State};
% tcp connection change to passive
handle_info({tcp_passive, Swocket}, #state{socket = Swocket} = State) ->
    setopts(Swocket),
    {noreply, State};
% connection closed
handle_info({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};
handle_info(Info, State) ->
    io:format("request received non_tcp: ~p.~n", [Info]),
    {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================

setopts(WsSwocket) ->
    inet:setopts(WsSwocket, [{active, 300}, {packet, 0}, binary]).