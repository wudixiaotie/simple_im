%% ===================================================================
%% Author xiaotie
%% 2016-4-16
%% session server worker
%% ===================================================================

-module(session_server_worker).

-behaviour(gen_msg).

% APIs
-export([start_link/1]).

% gen_msg callbacks
-export([init/1, handle_msg/2, terminate/2]).

-include("connection.hrl").

-record(state, {socket}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(Socket) ->
    gen_msg:start_link(?MODULE, [Socket], []).



%% ===================================================================
%% gen_msg callbacks
%% ===================================================================

init([Socket]) ->
    {ok, #state{socket = Socket}}.


handle_msg({tcp, Socket, [$r|T]}, State) ->
    [UserIdBin, PidBin] = re:split(T, ":"),
    UserIdStr = erlang:binary_to_list(UserIdBin),
    log:i("[Session] Session register UserId:~p~n", [UserIdStr]),
    Return = case dets:lookup(session, UserIdStr) of
        [] ->
            ?OK;
        [{_, OldPidBin}] ->
            OldPidBin
    end,
    ok = dets:insert(session, {UserIdStr, PidBin}),
    ok = gen_tcp:send(Socket, Return),
    {ok, State};
handle_msg({tcp, Socket, [$u|UserIdStr]}, State) ->
    log:i("[Session] Session unregister UserId:~p~n", [UserIdStr]),
    ok = dets:delete(session, UserIdStr),
    ok = gen_tcp:send(Socket, ?OK),
    {ok, State};
handle_msg({tcp, Socket, [$f|UserIdStr]}, State) ->
    Result = case dets:lookup(session, UserIdStr) of
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