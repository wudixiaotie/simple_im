%% ===================================================================
%% Author xiaotie
%% 2016-05-06
%% IM node monitor
%% ===================================================================

-module(im_node_monitor).

-behaviour(gen_msg).

% APIs
-export([start_link/0]).

% gen_msg callbacks
-export([init/1, handle_msg/2, terminate/2]).



%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    gen_msg:start_link({local, ?MODULE}, ?MODULE, [], []).



%% ===================================================================
%% gen_msg callbacks
%% ===================================================================

init([]) ->
    NodeFile = env:get(node_file),
    dets:open_file(node, [{access, read_write},
                          {auto_save, timer:minutes(1)},
                          {file, NodeFile},
                          {keypos, 1},
                          {ram_file, true},
                          {type, bag}]),
    {ok, []}.


handle_msg({nodeinit, Node}, State) ->
    true = erlang:monitor_node(Node, true),
    log:i("[session] Start monitor im node: ~p~n", [Node]),
    {ok, State};
handle_msg({nodedown, Node}, State) ->
    log:e("[session] IM node: ~p is down!~n", [Node]),
    log:i("[session] Clean expired session of IM node: ~p!~n", [Node]),
    ObjectList = dets:lookup(node, Node),
    ok = unregister_users(ObjectList),
    {ok, State};
handle_msg(Info, State) ->
    log:i("[Session] im_node_monitor got an unknown info: ~p.~n", [Info]),
    {ok, State}.


terminate(Reason, _State) ->
    log:e("[Session] im_node_monitor terminate with reason: ~p~n", [Reason]),
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================

unregister_users([{_, UserId} = H|T]) ->
    ok = dets:delete_object(node, H),
    ok = dets:delete(session, UserId),
    unregister_users(T);
unregister_users([]) ->
    ok.