%% ===================================================================
%% Author xiaotie
%% 2016-4-12
%% log server
%% ===================================================================

-module(log_server).

-export([start_link/0, init/0]).



%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    Pid = erlang:spawn_opt(log_server, init, [], [link, {fullsweep_after, 50}]),
    {ok, Pid}.


init() ->
    true = erlang:register(?MODULE, self()),

    LogDir = env:get(log_dir),
    case filelib:is_dir(LogDir) of
        false ->
            file:make_dir(LogDir);
        true ->
            ok
    end,
    AppMode = env:get(app_mode),
    AppModeStr = erlang:atom_to_list(AppMode),
    {ok, FileHandler} = file:open(LogDir ++ AppModeStr ++ ".log", [append]),
    
    Node = erlang:node(),
    loop(Node, FileHandler).



%% ===================================================================
%% Internal functions
%% ===================================================================

loop(Node, FileHandler) ->
    receive
        Log ->
            ok = file:write(FileHandler, Log)
    end,
    loop(Node, FileHandler).