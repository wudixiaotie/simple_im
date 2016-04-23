%% ===================================================================
%% Author xiaotie
%% 2016-4-22
%% session finder
%% ===================================================================

-module(session_finder).

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
    erlang:list_to_atom("session_finder_" ++ IndexStr).


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
            do_loop(Message, Name)
    end,
    loop(Name).


do_loop({find, From, UserId}, _) ->
    Result = case dets:lookup(session, UserId) of
        [] ->
            offline;
        [{UserId, Pid}] ->
            {ok, Pid}
    end,
    From ! {session, Result};
do_loop({stop, From}, Name) ->
    log:i("[Session] Session finder ~p stopped by admin~n", [Name]),
    From ! ok;
do_loop(Message, _) ->
    log:e("[IM] Session finder got unexpected message:~p~n", [Message]).