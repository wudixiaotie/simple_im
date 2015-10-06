%% ===================================================================
%% Author xiaotie
%% 2015-8-2
%% session store, search, synchronous from father node
%% ===================================================================

-module (session).

-behaviour (gen_server).

% APIs
-export([start_link/0, get_pid_list/1, register/2, unregister/1, verify/1]).

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


get_pid_list(UserId) ->
    case ets:lookup(session, UserId) of
        [] ->
            offline;
        [{UserId, DeviceList}] ->
            [Pid || {_, _, Pid} <- DeviceList]
    end.


register(User, Pid) ->
    Device = User#user.device,
    case utility:index_of(env:get(device_list), Device) of
        {ok, -1} ->
            ok = delete_token_from_redis(User#user.token),
            {error, <<"Wrong device">>};
        _ ->
            register(User#user.id, Device, User#user.token, Pid)
    end.
register(UserId, Device, Token, Pid) ->
    DeviceList = case ets:lookup(session, UserId) of
        [] ->
            [{Device, Token, Pid}];
        [{UserId, OriginalDeviceList}] ->
            case lists:keytake(Device, 1, OriginalDeviceList) of
                {value, {Device, OriginalToken, OriginalPid}, NewDeviceList} ->
                    ok = delete_token_from_redis(OriginalToken),
                    OriginalPid ! {replaced_by, Pid},
                    [{Device, Token, Pid}|NewDeviceList];
                false ->
                    [{Device, Token, Pid}|OriginalDeviceList]
            end
    end,

    % This place I use catch to ensure update_session will always 
    % be execuate wether session process is down or not.
    catch ets:insert(session, {UserId, DeviceList}),
    update_session(insert, {UserId, DeviceList}).


unregister(undefined) ->
    ok;
unregister(User) ->
    UserId = User#user.id,
    Device = User#user.device,
    DeviceList = case ets:lookup(session, UserId) of
        [] ->
            [];
        [{UserId, OriginalDeviceList}] ->
            ok = delete_token_from_redis(User#user.token),
            lists:keydelete(Device, 1, OriginalDeviceList)
    end,

    case DeviceList of
        [] ->
            catch ets:delete(session, UserId),
            update_session(delete, UserId);
        _ ->
            catch ets:insert(session, {UserId, DeviceList}),
            update_session(insert, {UserId, DeviceList})
    end.


verify(User) ->
    Device = User#user.device,
    case utility:index_of(env:get(device_list), Device) of
        {ok, -1} ->
            {error, <<"Wrong device">>};
        _ ->
            verify(User#user.id, Device, User#user.token)
    end.
verify(UserId, Device, Token) ->
    case ets:lookup(session, UserId) of
        [] ->
            offline;
        [{UserId, DeviceList}] ->
            case lists:keyfind(Device, 1, DeviceList) of
                {Device, Token, Pid} ->
                    {ok, Pid};
                _ ->
                    {error, <<"Token not match">>}
            end
    end.



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    ets:new(session, [named_table, public, {read_concurrency, true}, {write_concurrency, true}]),
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


delete_token_from_redis(Token) ->
    {ok, _} = redis:q([<<"DEL">>, redis:key({token, Token})]),
    ok.