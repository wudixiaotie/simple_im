%% ===================================================================
%% Author xiaotie
%% 2016-4-16
%% session worker
%% ===================================================================

-module(session_worker).

-behaviour(gen_msg).

% APIs
-export([start_link/2]).

% gen_msg callbacks
-export([init/1, handle_msg/2, terminate/2]).

-include("connection.hrl").

-record(state, {socket, table_ref}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(Socket, TableRef) ->
    gen_msg:start_link(?MODULE, [Socket, TableRef], []).



%% ===================================================================
%% gen_msg callbacks
%% ===================================================================

init([Socket, TableRef]) ->
    {ok, #state{socket = Socket, table_ref = TableRef}}.


handle_msg({tcp, Socket, [$r|T]}, State) ->
    [UserIdBin, PidBin] = re:split(T, ":"),
    UserIdStr = erlang:binary_to_list(UserIdBin),
    log:i("[Session] Session register UserId:~p~n", [UserIdStr]),
    Return = case dets:lookup(State#state.table_ref, UserIdStr) of
        [] ->
            ?OK;
        [{_, OldPidBin}] ->
            OldPidBin
    end,
    ok = dets:insert(State#state.table_ref, {UserIdStr, PidBin}),
    ok = gen_tcp:send(Socket, Return),
    {ok, State};
handle_msg({tcp, Socket, [$u|UserIdStr]}, State) ->
    log:i("[Session] Session unregister UserId:~p~n", [UserIdStr]),
    ok = dets:delete(State#state.table_ref, UserIdStr),
    ok = gen_tcp:send(Socket, ?OK),
    {ok, State};
handle_msg({tcp, Socket, [$f|UserIdStr]}, State) ->
    Result = case dets:lookup(State#state.table_ref, UserIdStr) of
        [] ->
            <<"offline">>;
        [{UserIdStr, PidBin}] ->
            PidBin
    end,
    ok = gen_tcp:send(Socket, Result),
    {ok, State};
handle_msg({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};
handle_msg(_Info, State) -> {ok, State}.


terminate(Reason, State) ->
    log:e("[Session] worker ~p terminate:~p~n", [self(), Reason]),
    gen_tcp:close(State#state.socket),
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================