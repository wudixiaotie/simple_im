%% ===================================================================
%% Author xiaotie
%% 2015-8-2
%% session store, search, synchronous from father node
%% ===================================================================

-module(session).

-behaviour(gen_server).

% APIs
-export([start_link/0, register/2, unregister/1, find/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).



%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


register(UserId, Pid) ->
    case ets:lookup(session, UserId) of
        [] ->
            ok;
        [{UserId, OldPid}] ->
            OldPid ! {replaced_by, Pid}
    end,

    Session = {UserId, Pid},

    % This place I use catch to ensure update_session will always 
    % be execuate wether session process is down or not.
    case catch ets:insert(session, Session) of
        true ->
            ok;
        Error ->
            log:e("session register error: ~p~n", [Error])
    end,
    update_session(insert, Session).


unregister(undefined) ->
    ok;
unregister(UserId) ->
    case catch ets:delete(session, UserId) of
        true ->
            ok;
        Error ->
            log:e("session unregister error: ~p~n", [Error])
    end,
    update_session(delete, UserId).


find(UserId) ->
    case ets:lookup(session, UserId) of
        [] ->
            offline;
        [{UserId, Pid}] ->
            {ok, Pid};
        Error ->
            {error, Error}
    end.



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    ets:new(session, [named_table,
                      public,
                      {read_concurrency, true},
                      {write_concurrency, true}]),
    Node = node(),
    case get_available_node() of
        {ok, Node} ->
            ignore;
        {ok, FatherNode} ->
            % The aim is to ensure that the message "{copy_from, FatherNode}" 
            % save at head of the session gen_server message queue, and 
            % register process name as soon as possible. Then current node 
            % will receive message when other node update session, so 
            % gen_server:"session" will process update session after 
            % copy session data from FatherNode, rather than the opposite.
            self() ! {copy_from, FatherNode};
        _ ->
            ignore
    end,
    {ok, []}.


handle_call(copy, _From, State) ->
    SessionList = ets:tab2list(session),
    {reply, SessionList, State};
handle_call(_Request, _From, State) ->
    {reply, nomatch, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({copy_from, FatherNode}, State) ->
    log:i("~p start copy session data from ~p~n", [node(), FatherNode]),
    SessionList = gen_server:call({?MODULE, FatherNode}, copy, infinity),
    init_session(SessionList),
    {noreply, State};
% {update, insert, {UserId, Pid}} | {update, delete, UserId}
handle_info({update, Type, Arg}, State) ->
    case catch ets:Type(session, Arg) of
        true ->
            ok;
        Error ->
            log:e("session update error: ~p~n", [Error])
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================

get_available_node() ->
    NodeList = env:get(node_list),
    get_available_node(NodeList).
get_available_node([H|T])  ->
    case net_adm:ping(H) of
        pong ->
            {ok, H};
        pang ->
            get_available_node(T)
    end;
get_available_node([]) ->
    {error, all_unavailable}.


init_session([H|T]) ->
    ets:insert(session, H),
    init_session(T);
init_session([]) ->
    ok.


update_session(Type, Arg) ->
    update_session(Type, Arg, nodes()).
update_session(Type, Arg, [H|T]) ->
    {?MODULE, H} ! {update, Type, Arg},
    update_session(Type, Arg, T);
update_session(_, _, []) ->
    ok.