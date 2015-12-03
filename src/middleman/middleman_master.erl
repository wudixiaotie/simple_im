%% ===================================================================
%% Author xiaotie
%% 2015-12-03
%% middleman work for master
%% ===================================================================

-module(middleman_master).

-behaviour(gen_server).

% APIs
-export([start_link/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, {socket, hunter_count}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Socket]) ->
    ok = inet:setopts(Socket, [{active, true}, {packet, 0}, binary]),
    {ok, HunterCount} = hunter_count(),
    {ok, #state{socket = Socket, hunter_count = HunterCount}}.


handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.


handle_info({tcp, _Socket, Bin}, State) ->
    {ok, HunterName} = look_for_hunter(State#state.hunter_count),
    HunterName ! {job, Bin},
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};
handle_info(_Info, State) -> {noreply, State}.


terminate(Reason, _State) ->
    log:e("[Middleman] Middleman work for master has down! Reason: ~p~n", [Reason]),
    ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

hunter_count() ->
    hunter_count(1, normal).
hunter_count(N, max) ->
    {ok, HunterName} = middleman_helper:hunter_name(N),
    case erlang:whereis(HunterName) of
        undefined ->
            {ok, N};
        _ ->
            hunter_count(N + 1, normal)
    end;
hunter_count(N, normal) ->
    {ok, HunterName} = middleman_helper:hunter_name(N),
    case erlang:whereis(HunterName) of
        undefined ->
            hunter_count(N + 1, max);
        _ ->
            hunter_count(N + 1, normal)
    end.


look_for_hunter(HunterCount) ->
    look_for_hunter(HunterCount, 0).
look_for_hunter(HunterCount, Times) when Times < 3 ->
    {ok, Index} = utility:random_number(HunterCount),
    {ok, HunterName} = middleman_helper:hunter_name(Index),
    case erlang:whereis(HunterName) of
        undefined ->
            look_for_hunter(Times + 1);
        _ ->
            {ok, HunterName}
    end;
look_for_hunter(_, _) ->
    {error, nil}.