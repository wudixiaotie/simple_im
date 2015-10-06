%% ===================================================================
%% Author xiaotie
%% 2015-8-2
%% server for client side
%% ===================================================================

-module (client).

-behaviour(gen_server).

% APIs
-export([start_link/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record(state, {socket,
                heartbeat_timeout,
                user,
                msg_cache}). %% [{MsgId, MsgBin},...]

-include("user.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

start_link(Socket, User) ->
    gen_server:start_link(?MODULE, [Socket, User], []).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Socket, User]) ->
    session:register(User, self()),
    RR = <<"[rr] id=\"a_01\" t=\"login\" s=0">>,
    gen_tcp:send(Socket, RR),
    State = #state{socket = Socket,
                   heartbeat_timeout = env:get(heartbeat_timeout),
                   user = User,
                   msg_cache = []},
    setopts(State#state.socket),
    {ok, State, State#state.heartbeat_timeout}.


handle_call(_Request, _From, State) ->
    {reply, nomatch, State, State#state.heartbeat_timeout}.
handle_cast(_Msg, State) ->
    {noreply, State, State#state.heartbeat_timeout}.



%% ===================================================================
%% socket
%% ===================================================================

handle_info({new_socket, NewSocket}, #state{socket = Socket} = State) ->
    gen_tcp:close(Socket),
    ok = clean_mailbox(Socket),
    setopts(NewSocket),
    {noreply, State#state{socket = NewSocket}, State#state.heartbeat_timeout};
handle_info({tcp, Socket, Data}, #state{socket = Socket} = State) ->
    {ok, Toml} = etoml:parse(Data),
    {ok, NewState} = process_packet(Toml, State),
    {noreply, NewState, NewState#state.heartbeat_timeout};
% tcp connection change to passive
handle_info({tcp_passive, Socket}, #state{socket = Socket} = State) ->
    setopts(Socket),
    {noreply, State, State#state.heartbeat_timeout};
% connection closed
handle_info({tcp_closed, _Socket}, State) ->
    {noreply, State#state{socket = undefined}, State#state.heartbeat_timeout};



%% ===================================================================
%% business receiver
%% ===================================================================

handle_info({msg_pack, {MsgId, MsgWithTs}}, State) ->
    {ok, MsgBin} = utility:tuple_to_toml(MsgWithTs),
    gen_tcp:send(State#state.socket, MsgBin),
    NewMsgCache = [{MsgId, MsgBin}|State#state.msg_cache],
    NewState = State#state{msg_cache = NewMsgCache},
    {noreply, NewState, NewState#state.heartbeat_timeout};



%% ===================================================================
%% client logic functions
%% ===================================================================

handle_info({be_replaced, NewPid}, State) ->
    NewPid ! {msg_cache, State#state.msg_cache},
    {stop, {be_replaced, NewPid}, State};
handle_info({msg_cache, OriginalMsgCache}, #state{msg_cache = MsgCache} = State) ->
    NewState = State#state{msg_cache = MsgCache ++ OriginalMsgCache},
    {noreply, NewState, NewState#state.heartbeat_timeout};
handle_info(timeout, State) ->
    UserId = (State#state.user)#user.id,
    offline:store(UserId, State#state.msg_cache),
    proc_lib:hibernate(gen_server, enter_loop, [?MODULE, [], State]),
    {noreply, State, State#state.heartbeat_timeout};
handle_info(Info, State) ->
    log:i("Unknown Info: ~p.~n", [Info]),
    {noreply, State, State#state.heartbeat_timeout}.


terminate(Reason, #state{msg_cache = MsgCache, user = User}) ->
    log:i("Client terminate with reason: ~p~n", [Reason]),
    offline:store(User#user.id, MsgCache),
    session:unregister(User).
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

setopts(Socket) ->
    inet:setopts(Socket, [{active, 300}, {packet, 0}, binary]).


clean_mailbox(Socket) ->
    receive
        {tcp, Socket, _} ->
            clean_mailbox(Socket);
        {tcp_passive, Socket} ->
            clean_mailbox(Socket);
        {tcp_closed, Socket} ->
            clean_mailbox(Socket)
    after
        0 ->
            ok
    end.


% request
process_packet([{<<"r">>, Attrs}|T], #state{socket = Socket} = State) ->
    {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
    log:i("Got r id=~p~n", [MsgId]),
    case lists:keyfind(<<"t">>, 1, Attrs) of
        _ ->
            RR = <<"[rr] id = \"", MsgId/binary, "\" c = \"Unknown request\"">>,
            gen_tcp:send(Socket, RR),
            process_packet(T, State)
    end;
% message
process_packet([{<<"m">>, Attrs} = Msg|T], #state{socket = Socket} = State) ->
    MsgId = send_ack(Socket, Attrs),
    MsgWithTs = add_timestamp(Msg),
    case lists:keyfind(<<"to">>, 1, Attrs) of
        {<<"to">>, ToUserInfo} ->
            {ok, ToUser} = users:parse(ToUserInfo),
            MsgPack = {MsgId, MsgWithTs},
            send_msg_2_single_user(ToUser#user.id, MsgPack);
        _ ->
            ignore
    end,
    process_packet(T, State);
% group message
process_packet([{<<"gm">>, Attrs} = Msg|T], #state{socket = Socket} = State) ->
    MsgId = send_ack(Socket, Attrs),
    MsgWithTs = add_timestamp(Msg),
    case lists:keyfind(<<"group">>, 1, Attrs) of
        {<<"group">>, [{<<"id">>, GroupId}]} ->
            {ok, UserIdList} = groups:get_user_id_list(GroupId),
            MsgPack = {MsgId, MsgWithTs},
            ok = send_msg_2_multiple_users(UserIdList, MsgPack);
        _ ->
            ignore
    end,
    process_packet(T, State);
% ack
process_packet([{<<"a">>, Attrs}|T], State) ->
    {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
    NewMsgCache = lists:keydelete(MsgId, 1, State#state.msg_cache),
    process_packet(T, State#state{msg_cache = NewMsgCache});
process_packet([], NewState) ->
    {ok, NewState}.


send_ack(Socket, Attrs) ->
    {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
    Ack = <<"[a] id = \"", MsgId/binary, "\"">>,
    log:i("Got msg id=~p~n", [MsgId]),
    gen_tcp:send(Socket, Ack),
    MsgId.


add_timestamp({Type, Attrs}) ->
    Timestamp = utility:timestamp(),
    NewAttrs = lists:keystore(<<"ts">>, 1, Attrs, {<<"ts">>, Timestamp}),
    {Type, NewAttrs}.


send_msg_2_single_user(UserId, MsgPack) ->
    case session:get_pid_list(UserId) of
        offline ->
            offline:store(UserId, [MsgPack]),
            log:i("offline msg: ~p~n", [MsgPack]),
            ok;
        ToPidList ->
            send_msg_to_pid(ToPidList, MsgPack)
    end.


send_msg_2_multiple_users([H|T], MsgPack) ->
    send_msg_2_single_user(H, MsgPack),
    send_msg_2_multiple_users(T, MsgPack);
send_msg_2_multiple_users([], _) ->
    ok.


send_msg_to_pid([H|T], MsgPack) ->
    H ! {msg_pack, MsgPack},
    send_msg_to_pid(T, MsgPack);
send_msg_to_pid([], _) ->
    ok.