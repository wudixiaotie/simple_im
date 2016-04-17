%% ===================================================================
%% Author xiaotie
%% 2016-4-16
%% session worker
%% ===================================================================

-module(session_worker).

-behaviour(gen_msg).

% APIs
-export([start_link/1]).

% gen_msg callbacks
-export([init/1, handle_msg/2, terminate/2]).

-include("connection.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

start_link(Socket) ->
    gen_msg:start_link(?MODULE, [Socket], []).



%% ===================================================================
%% gen_msg callbacks
%% ===================================================================

init([Socket]) ->
    ok = inet:setopts(Socket, [{active, true}, {packet, 0}, list]),
    {ok, []}.


handle_msg({tcp, Socket, [$r|T]}, State) ->
    [UserIdBin, PidBin] = re:split(T, ":"),
    UserIdStr = erlang:binary_to_list(UserIdBin),
    ok = ets:insert(session, {UserIdStr, PidBin}),
    ok = gen_tcp:send(Socket, ?OK),
    {ok, State};
%     case ets:lookup(session, UserId) of
%         [] ->
%             ok;
%         [{UserId, OldPid}] ->
%             OldPid ! {replaced_by, Pid}
%     end,

%     Session = {UserId, Pid},

%     % This place I use catch to ensure update_session will always 
%     % be execuate wether session process is down or not.
%     case catch ets:insert(session, Session) of
%         true ->
%             ok;
%         Error ->
%             log:e("[IM] Session register error: ~p~n", [Error])
%     end,
%     update_session(insert, Session).
handle_msg({tcp, Socket, [$u|UserIdStr]}, State) ->
    true = ets:delete(session, UserIdStr)
    ok = gen_tcp:send(Socket, ?OK),
    {ok, State};
handle_msg({tcp, Socket, [$f|UserIdStr]}, State) ->
    Result = case ets:lookup(session, UserIdStr) of
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


terminate(Reason, _State) ->
    io:format("terminate:~p~n", [Reason]),
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================