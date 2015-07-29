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
    {ok, State}.
handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.


handle_info ({tcp, Socket, Data}, #state{socket = Socket} = State) ->
    io:format ("Got msg======~p~n", [Data]),
    {ok, Toml} = etoml:parse(Data),
    [{<<"msg">>, Attrs}] = Toml,
    {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
    Ack = <<"[ack] id=\"", MsgId/binary, "\"">>,
    io:format ("Send ack======~p~n", [Ack]),
    gen_tcp:send(Socket, Ack),
    {noreply, State};
handle_info ({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, NewState};
handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================