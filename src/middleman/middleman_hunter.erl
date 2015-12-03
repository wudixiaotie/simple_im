%% ===================================================================
%% Author xiaotie
%% 2015-12-03
%% middleman work for hunter
%% ===================================================================

-module(middleman_hunter).

-behaviour(gen_server).

% APIs
-export([start_link/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {name, socket}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Socket]) ->
    {ok, HunterName} = register_hunter(),
    ok = inet:setopts(Socket, [{active, true}, {packet, 0}, binary]),
    {ok, #state{name = HunterName, socket = Socket}}.


handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.


handle_info({job, Bin}, State) ->
    ok = gen_tcp:send(State#state.socket, Bin),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};
handle_info(_Info, State) -> {noreply, State}.


terminate(Reason, State) ->
    log:e("[Middleman] Middleman worker ~p has down! Reason: ~p~n", [State#state.name, Reason]),
    ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

register_hunter() ->
    register_hunter(1).
register_hunter(N) ->
    {ok, HunterName} = middleman_helper:hunter_name(N),
    case erlang:whereis(HunterName) of
        undefined ->
            erlang:register(HunterName, self()),
            {ok, HunterName};
        _ ->
            register_hunter(N + 1)
    end.