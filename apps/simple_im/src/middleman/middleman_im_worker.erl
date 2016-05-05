%% ===================================================================
%% Author xiaotie
%% 2015-12-03
%% middleman work for im
%% ===================================================================

-module(middleman_im_worker).

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
    {ok, IMWorkerName} = register_im_worker(),
    {ok, #state{name = IMWorkerName, socket = Socket}}.


handle_msg({notify, Bin}, State) ->
    ok = gen_tcp:send(State#state.socket, Bin),
    {ok, State};
handle_msg({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};
handle_msg(_Info, State) -> {ok, State}.


terminate(Reason, State) ->
    ok = notify_all_http_worker(im_worker_terminate),
    log:e("[Middleman] Middleman im worker ~p has down! Reason: ~p~n", [State#state.name, Reason]),
    gen_tcp:close(State#state.socket),
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================

register_im_worker() ->
    register_im_worker(1).
register_im_worker(N) ->
    {ok, IMWorkerName} = middleman_helper:im_worker_name(N),
    case erlang:whereis(IMWorkerName) of
        undefined ->
            erlang:register(IMWorkerName, self()),
            ok = notify_all_http_worker(im_worker_init),
            {ok, IMWorkerName};
        _ ->
            register_im_worker(N + 1)
    end.


notify_all_http_worker(Content) ->
    Names = erlang:registered(),
    notify_all_http_worker(Names, Content).
notify_all_http_worker([H|T], Content) ->
    case erlang:atom_to_list(H) of
        [$h, $t, $t, $p, $_|_] ->
            H ! Content;
        _ ->
            ok
    end,
    notify_all_http_worker(T, Content);
notify_all_http_worker([], _) ->
    ok.