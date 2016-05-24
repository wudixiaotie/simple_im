-module (udp_server).

-behaviour (gen_server).

-export ([start_link/0]).

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
          code_change/3]).

-record(state, {socket}).

-define (ACTIVE_NUM, 2000).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    {ok, Socket} = gen_udp:open(20844, [binary, {active, ?ACTIVE_NUM}]),
    {ok, #state{socket = Socket}}.

handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info({udp, Socket, IP, InPortNo, Packet}, State) ->
    io:format("==Server received: Packet:~p~n~p~n", [Packet, {Socket, IP, InPortNo}]),
    ok = gen_udp:send(Socket, IP, InPortNo, <<"Got your packet, fucker!">>),
    {noreply, State};
handle_info({udp_passive, Socket}, State) ->
    ok = inet:setopt(Socket, {active, ?ACTIVE_NUM}),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.