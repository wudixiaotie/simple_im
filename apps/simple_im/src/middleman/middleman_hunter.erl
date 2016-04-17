%% ===================================================================
%% Author xiaotie
%% 2015-12-03
%% middleman work for hunter
%% ===================================================================

-module(middleman_hunter).

-behaviour(gen_msg).

% APIs
-export([start_link/1]).

% gen_msg callbacks
-export([init/1, handle_msg/2, terminate/2]).


-record(state, {name, socket}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(Socket) ->
    gen_msg:start_link(?MODULE, [Socket], []).



%% ===================================================================
%% gen_msg callbacks
%% ===================================================================

init([Socket]) ->
    {ok, HunterName} = register_hunter(),
    ok = inet:setopts(Socket, [{active, true}, {packet, 0}, binary]),
    {ok, #state{name = HunterName, socket = Socket}}.


handle_msg({job, Bin}, State) ->
    ok = gen_tcp:send(State#state.socket, Bin),
    {ok, State};
handle_msg({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};
handle_msg(_Info, State) -> {ok, State}.


terminate(Reason, State) ->
    ok = notify_all_master(hunter_terminate),
    log:e("[Middleman] Middleman worker ~p has down! Reason: ~p~n", [State#state.name, Reason]),
    ok.



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
            ok = notify_all_master(hunter_init),
            {ok, HunterName};
        _ ->
            register_hunter(N + 1)
    end.


notify_all_master(Content) ->
    Names = erlang:registered(),
    notify_all_master(Names, Content).
notify_all_master([H|T], Content) ->
    case erlang:atom_to_list(H) of
        [$m, $a, $s, $t, $e, $r, $_|_] ->
            H ! Content;
        _ ->
            ok
    end,
    notify_all_master(T, Content);
notify_all_master([], _) ->
    ok.