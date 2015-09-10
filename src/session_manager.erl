-module (session_manager).

-behaviour (gen_server).

% APIs
-export([start_link/0, get/1, register/2, unregister/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-include("user.hrl").

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
        [{UserId, DPList}] ->
            [Pid || {_, Pid} <- DPList]
    end.

register(User, Pid) ->
    UserId = User#user.id,
    UserDevice = User#user.device,
    DPList = case ets:lookup(session, UserId) of
        [] ->
            [{UserDevice, Pid}];
        [{UserId, OriginalDPList}] ->
            lists:keyreplace(UserDevice, 1, OriginalDPList, {UserDevice, Pid})
    end,

    % This place I use catch to ensure update_session will always 
    % be execuate wether session process is down or not.
    catch ets:insert(session, {UserId, DPList}),
    update_session(insert, {UserId, DPList}).

unregister(User) ->
    UserId = User#user.id,
    UserDevice = User#user.device,
    DPList = case ets:lookup(session, UserId) of
        [] ->
            [];
        [{UserId, OriginalDPList}] ->
            lists:keydelete(UserDevice, 1, OriginalDPList)
    end,

    case DPList of
        [] ->
            catch ets:delete(session, UserId),
            update_session(delete, UserId);
        _ ->
            catch ets:insert(session, {UserId, DPList}),
            update_session(insert, {UserId, DPList})
    end.



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
            net_adm:ping(FatherNode),
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
    {?MODULE, H} ! {update, Type, Session},
    update_session(Type, Session, T);
update_session(_Type, _Session, []) ->
    ok.