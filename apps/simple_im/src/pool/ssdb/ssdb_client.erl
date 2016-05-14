%% ===================================================================
%% Author xiaotie
%% 2016-05-14
%% ssdb client
%% ===================================================================

-module(ssdb_client).

-behaviour(gen_server).

% APIs
-export([start_link/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {socket}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    [{host, SSDBHost}, {port, SSDBPort}] = env:get(ssdb),
    {ok, Socket} = gen_tcp:connect(SSDBHost, SSDBPort, [{active, false}, {packet, 0}, binary]),
    {ok, #state{socket = Socket}}.


handle_call({q, Packet}, _From, #state{socket = Socket} = State) ->
    ok = gen_tcp:send(Socket, Packet),
    case gen_tcp:recv(Socket, 0) of
        {ok, Result} ->
            {reply, Result, State};
        {error,closed} ->
            {stop, tcp_closed, error, State}
    end;
handle_call(_Request, _From, State) -> {reply, nomatch, State}.


handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.


terminate(Reason, State) ->
    log:e("[SSDB] SSDB client is down! Reason: ~p~n", [Reason]),
    ok = gen_tcp:closed(State#state.socket),
    ok.


code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================