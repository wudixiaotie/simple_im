%% ===================================================================
%% Author xiaotie
%% 2015-11-19
%% agent for middleman
%% ===================================================================

-module(agent).

-behaviour(gen_server).

% APIs
-export([for_master/0, for_hunter/0, offer_a_reward/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {socket, role}).

-include("connection.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

for_master() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [master], []).


for_hunter() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [hunter], []).


offer_a_reward(_Msg) ->
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
            {ok, #state{socket = Socket, role = Role}}
    after
        1000 ->
            log:e("[Middleman] Agent init failed~n"),
            ok = gen_tcp:close(Socket),
            {stop, connect_failed}
    end.


handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.


handle_info({tcp, _Socket, _Data}, #state{role = hunter} = State) ->
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};
handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================