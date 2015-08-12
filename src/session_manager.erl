-module (session).

-behaviour (gen_server).

% APIs
-export([start_link/0, get/1, register/2, unregister/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

%% ===================================================================
%% Type spec
%% ===================================================================

-type userid() :: binary().


%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(UserId) ->
    case ets:lookup(session, UserId) of
        [] ->
            offline;
        [{UserId, Pid}] ->
            Pid
    end.

register(UserId, Pid) ->
    % This place I use catch to ensure update_session will always 
    % be execuate wether session process is down or not.
    catch ets:insert(session, {UserId, Pid}),
    update_session(insert, {UserId, Pid}).

unregister(UserId) ->
    catch ets:delete(session, UserId),
    update_session(delete, UserId).

    

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    ets:new(session, [named_table, public, {read_concurrency, true}, {write_concurrency, true}]),
    FatherNode = env:get(father_node),
    case FatherNode == node() of
        true ->
            ok;
        false ->
            % The aim is to ensure that the message "{copy_from, FatherNode}" 
            % save at head of the session gen_server message queue, and 
            % register process name as soon as possible. Then current node 
            % will receive message when other node update session, so 
            % gen_server:"session" will process update session after 
            % copy session data from FatherNode, rather than the opposite.
            self() ! {copy_from, FatherNode}
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
handle_info({update, Type, Session}, State) ->
    ets:Type(session, Session),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================

init_session([H|T]) ->
    ets:insert(session, H),
    init_session(T);
init_session([]) ->
    ok.

-spec update_session(Type :: insert | delete,
                     Session :: userid() |
                                {userid(), pid()}) -> ok.
update_session(Type, Session) ->
    update_session(Type, Session, nodes()).
update_session(Type, Session, [H|T]) ->
    {session, H} ! {update, Type, Session},
    update_session(Type, Session, T);
update_session(_Type, _Session, []) ->
    ok.