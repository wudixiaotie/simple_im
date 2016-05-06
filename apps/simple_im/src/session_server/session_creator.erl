%% ===================================================================
%% Author xiaotie
%% 2016-4-23
%% session creator
%% ===================================================================

-module(session_creator).

% APIs
-export([start_link/1, name/1, init/1]).

-include("connection.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

start_link(Index) ->
    Pid = erlang:spawn_opt(?MODULE, init, [Index], [link]),
    {ok, Pid}.


name(Index) ->
    IndexStr = erlang:integer_to_list(Index),
    erlang:list_to_atom("session_creator_" ++ IndexStr).


init(Index) ->
    Name = name(Index),
    true = erlang:register(Name, self()),
    loop(Name).



%% ===================================================================
%% Internal functions
%% ===================================================================

loop(Name) ->
    receive
        Message -> 
            case catch do_loop(Message, Name) of
                {'EXIT', Reason} ->
                    log:e("[Session] worker ~p terminate:~p~n", [self(), Reason]);
                _ ->
                    ok
            end
    end,
    loop(Name).


do_loop({register, From, UserId, Pid}, _) ->
    log:i("[Session] Session register UserId:~p~n", [UserId]),

    Node = erlang:node(Pid),
    ok = dets:insert(node, {Node, UserId}),

    Result = case dets:lookup(session, UserId) of
        [] ->
            ok;
        [{_, OldPid}] ->
            OldNode = erlang:node(OldPid),
            ok = dets:delete_object(node, {OldNode, UserId}),
            OldPid
    end,
    ok = dets:insert(session, {UserId, Pid}),
    From ! {session, Result};
do_loop({unregister, From, UserId}, _) ->
    log:i("[Session] Session unregister UserId:~p~n", [UserId]),

    case dets:lookup(session, UserId) of
        [] ->
            ok;
        [{_, OldPid}] ->
            Node = erlang:node(OldPid),
            ok = dets:delete_object(node, {Node, UserId})
    end,

    ok = dets:delete(session, UserId),
    From ! {session, ok};
do_loop({stop, From}, Name) ->
    log:i("[Session] Session creator ~p stopped by admin~n", [Name]),
    From ! ok;
do_loop(Message, _) ->
    log:e("[IM] Session creator got unexpected message:~p~n", [Message]).