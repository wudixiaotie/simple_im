-module (udp_client).

-behaviour (gen_server).

-export ([start_link/0, send/1]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
          code_change/3]).

-record(state, {socket}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


send(Msg) ->
    ok = gen_server:call(?MODULE, {send, Msg}).


init([]) ->
    {ok, Socket} = gen_udp:open(20845, [binary, {active, true}]),
    {ok, #state{socket = Socket}}.

handle_call({send, Msg}, _From, State) ->
    ok = gen_udp:send(State#state.socket, "localhost", 20844, Msg),
    {reply, ok, State};
handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info({udp, Socket, IP, InPortNo, Packet}, State) ->
    io:format("==Client received: Packet:~p~n~p~n", [Packet, {Socket, IP, InPortNo}]),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.