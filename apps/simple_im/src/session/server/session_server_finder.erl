%% ===================================================================
%% Author xiaotie
%% 2016-4-22
%% session server finder
%% ===================================================================

-module(session_server_finder).

% APIs
-export([start_link/1, init/1]).

-include("connection.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

start_link(Index) ->
    Pid = erlang:spawn_opt(?MODULE, init, [Index], [link]),
    {ok, Pid}.


init(Index) ->
    Name = session_finder:name(Index),
    true = erlang:register(Name, self()),
    loop().



%% ===================================================================
%% Internal functions
%% ===================================================================

loop() ->
    receive
        Message -> 
            do_loop(Message)
    end,
    loop().


do_loop({find, From, UserId}) ->
    Result = case dets:lookup(session, UserId) of
        [] ->
            offline;
        [{UserId, Pid}] ->
            {ok, Pid}
    end,
    From ! {session, Result};
do_loop(Message) ->
    log:e("[IM] Session finder got unexpected message:~p~n", [Message]).