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


-record(state, {name, socket, hunter_count}).



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
    {ok, MasterName} = register_master(),
    {ok, HunterCount} = hunter_count(),
    {ok, #state{name = MasterName, socket = Socket, hunter_count = HunterCount}}.


handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.


handle_info({tcp, _Socket, Bin}, State) ->
    {ok, HunterName} = look_for_hunter(State#state.hunter_count),
    HunterName ! {job, Bin},
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};
handle_info(hunter_init, State) ->
    HunterCount = State#state.hunter_count,
    {noreply, State#state{hunter_count = HunterCount + 1}};
handle_info(hunter_terminate, State) ->
    HunterCount = State#state.hunter_count,
    {noreply, State#state{hunter_count = HunterCount - 1}};
handle_info(_Info, State) -> {noreply, State}.


terminate(Reason, _State) ->
    log:e("[Middleman] Middleman work for master has down! Reason: ~p~n", [Reason]),
    ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

hunter_count() ->
    Names = erlang:registered(),
    hunter_count(Names, 0).
hunter_count([H|T], HunterCount) ->
    case erlang:atom_to_list(H) of
        [$h, $u, $n, $t, $e, $r, $_|_] ->
            hunter_count(T, HunterCount + 1);
        _ ->
            hunter_count(T, HunterCount)
    end;
hunter_count([], HunterCount) ->
    {ok, HunterCount}.


register_master() ->
    register_master(1).
register_master(N) ->
    {ok, MasterName} = middleman_helper:master_name(N),
    case erlang:whereis(MasterName) of
        undefined ->
            erlang:register(MasterName, self()),
            {ok, MasterName};
        _ ->
            register_master(N + 1)
    end.


look_for_hunter(HunterCount) ->
    look_for_hunter(HunterCount, 0).
look_for_hunter(HunterCount, Times) when Times < 3 ->
    {ok, Index} = utility:random_number(HunterCount),
    {ok, HunterName} = middleman_helper:hunter_name(Index),
    case erlang:whereis(HunterName) of
        undefined ->
            look_for_hunter(HunterCount, Times + 1);
        _ ->
            {ok, HunterName}
    end;
look_for_hunter(_, _) ->
    {error, nil}.