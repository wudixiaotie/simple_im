%% ===================================================================
%% Author xiaotie
%% 2015-12-03
%% middleman work for http
%% ===================================================================

-module(middleman_http_worker).

-behaviour(gen_msg).

% APIs
-export([start_link/1]).

% gen_msg callbacks
-export([init/1, handle_msg/2, terminate/2]).


-record(state, {name, socket, im_worker_count}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(Socket) ->
    gen_msg:start_link(?MODULE, [Socket], []).



%% ===================================================================
%% gen_msg callbacks
%% ===================================================================

init([Socket]) ->
    {ok, HttpWorkerName} = register_http_worker(),
    {ok, IMWorkerCount} = im_worker_count(),
    {ok, #state{name = HttpWorkerName, socket = Socket, im_worker_count = IMWorkerCount}}.


handle_msg({tcp, _Socket, Bin}, State) ->
    {ok, IMWorkerName} = look_for_im_worker(State#state.im_worker_count),
    IMWorkerName ! {notify, Bin},
    {ok, State};
handle_msg({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};
handle_msg(im_worker_init, State) ->
    IMWorkerCount = State#state.im_worker_count,
    {ok, State#state{im_worker_count = IMWorkerCount + 1}};
handle_msg(im_worker_terminate, State) ->
    IMWorkerCount = State#state.im_worker_count,
    {ok, State#state{im_worker_count = IMWorkerCount - 1}};
handle_msg(_Info, State) -> {ok, State}.


terminate(Reason, State) ->
    log:e("[Middleman] Middleman http worker ~p has down! Reason: ~p~n", [State#state.name, Reason]),
    gen_tcp:close(State#state.socket),
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================

im_worker_count() ->
    Result = supervisor:count_children(middleman_im_worker_sup),
    {active, IMWorkerCount} = lists:keyfind(active, 1, Result),
    {ok, IMWorkerCount}.


register_http_worker() ->
    register_http_worker(1).
register_http_worker(N) ->
    {ok, HttpWorkerName} = middleman_helper:http_worker_name(N),
    case erlang:whereis(HttpWorkerName) of
        undefined ->
            erlang:register(HttpWorkerName, self()),
            {ok, HttpWorkerName};
        _ ->
            register_http_worker(N + 1)
    end.


look_for_im_worker(IMWorkerCount) ->
    {ok, Index} = utility:random_number(IMWorkerCount),
    {ok, IMWorkerName} = middleman_helper:im_worker_name(Index),
    case erlang:whereis(IMWorkerName) of
        undefined ->
            IMWorkerList = supervisor:which_children(middleman_im_worker_sup),
            {ok, NewIndex} = utility:random_number(erlang:length(IMWorkerList)),
            NewIMWorkerName = lists:nth(NewIndex, IMWorkerList),
            {ok, NewIMWorkerName};
        _ ->
            {ok, IMWorkerName}
    end.