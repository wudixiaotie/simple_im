%% ===================================================================
%% Author xiaotie
%% 2016-4-23
%% session server
%% ===================================================================

-module(session_server).

% APIs
-export([start/0, stop/0]).



%% ===================================================================
%% APIs
%% ===================================================================

start() ->
    SessionFile = env:get(session_file),
    dets:open_file(session, [{access, read_write},
                             {auto_save, timer:minutes(1)},
                             {file, SessionFile},
                             {keypos, 1},
                             {ram_file, true}]),

    % recover data
    case filelib:is_file(SessionFile) of
        true ->
            {ok, OldTableRef} = dets:open_file(SessionFile),
            Start = dets:bchunk(OldTableRef, start),
            Input = init_bchunk(OldTableRef, Start),
            ok = dets:init_table(session, Input, [{format, bchunk}]);
        false ->
            ok
    end,

    SessionFinderSize = env:get(session_finder_size),
    ok = start_worker(session_finder_sup, SessionFinderSize),
    SessionCreatorSize = env:get(session_creator_size),
    ok = start_worker(session_creator_sup, SessionCreatorSize),
    ok.


stop() ->
    ok = dets:close(session),
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================

init_bchunk(Tab, State) ->
    fun(read) when State =:= '$end_of_table' ->
        end_of_input;
       (read) when element(1, State) =:= error ->
        State;
       (read) ->
        {Cont, Objs} = State,
        {Objs, init_bchunk(Tab, dets:bchunk(Tab, Cont))};
       (close) ->
        ok
    end.


start_worker(_, 0) ->
    ok;
start_worker(SupName, Index) ->
    supervisor:start_child(SupName, [Index]),
    start_worker(SupName, Index - 1).