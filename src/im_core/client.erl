%% ===================================================================
%% Author xiaotie
%% 2015-8-2
%% server for client side
%% ===================================================================

-module (client).

-behaviour(gen_server).

% APIs
-export([start_link/3]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {heartbeat_timeout,
                user_id,
                device_list,
                msg_cache}). %% [#message{},...]

-include("device.hrl").
-include("message.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

start_link(Message, UserId, Device) ->
    gen_server:start_link(?MODULE, [Message, UserId, Device], []).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Message, UserId, #device{socket = Socket} = Device]) ->
    session:register(UserId, self()),
    ok = gen_tcp:send(Device#device.socket, Message#message.bin),
    State = #state{heartbeat_timeout = env:get(heartbeat_timeout),
                   user_id = UserId,
                   device_list = [Device],
                   msg_cache = []},
    setopts(Socket),
    {ok, State, State#state.heartbeat_timeout}.


handle_call(_Request, _From, State) ->
    {reply, nomatch, State, State#state.heartbeat_timeout}.
handle_cast(_Msg, State) ->
    {noreply, State, State#state.heartbeat_timeout}.



%% ===================================================================
%% socket
%% ===================================================================

handle_info({replace_socket, Message, #device{socket = Socket} = Device},
            #state{device_list = DeviceList} = State) ->
    ok = gen_tcp:send(Socket, Message#message.bin),

    NewState = case lists:keytake(Device#device.name,
                                  #device.name, DeviceList) of
        {value, #device{socket = OldSocket} = OldDevice, OtherDeivces} ->
            ok = gen_tcp:close(OldSocket),
            ok = clean_mailbox(OldSocket),
            ok = delete_useless_token([OldDevice]),

            OldDeviceMsgCache = OldDevice#device.msg_cache,
            NewDevice = Device#device{msg_cache = OldDeviceMsgCache},
            ok = send_msg_cache([NewDevice]),

            State#state{device_list = [NewDevice|OtherDeivces]};
        false ->
            State#state{device_list = [Device|DeviceList]}
    end,
    setopts(Socket),
    {noreply, NewState, NewState#state.heartbeat_timeout};
handle_info({tcp, Socket, Data}, State) ->
    {ok, Toml} = toml:binary_2_term(Data),
    {ok, NewState} = process_packet(Toml, Socket, State),
    {noreply, NewState, NewState#state.heartbeat_timeout};
% tcp connection change to passive
handle_info({tcp_passive, Socket}, State) ->
    setopts(Socket),
    {noreply, State, State#state.heartbeat_timeout};
% connection closed
handle_info({tcp_closed, _Socket}, State) ->
    {noreply, State, State#state.heartbeat_timeout};



%% ===================================================================
%% business receiver
%% ===================================================================

handle_info(Message, State) when is_record(Message, message) ->
    {ok, NewState} = send_msg_2_multiple_device(State#state.device_list,
                                                Message,
                                                State,
                                                save),
    {noreply, NewState, NewState#state.heartbeat_timeout};



%% ===================================================================
%% client logic functions
%% ===================================================================

handle_info({replaced_by, NewPid}, State) ->
    NewPid ! {msg_cache, State#state.msg_cache},
    {stop, {replaced_by, NewPid}, State};
handle_info({msg_cache, OriginalMsgCache}, State) ->
    case OriginalMsgCache of
        [] ->
            NewState = State;
        _ ->

            {ok, NewState} = merge_msg_cache(State, OriginalMsgCache)
    end,
    {noreply, NewState, NewState#state.heartbeat_timeout};
handle_info(timeout, State) ->
    ok = offline:store(State#state.user_id, State#state.msg_cache),
    proc_lib:hibernate(gen_server, enter_loop, [?MODULE, [], State]),
    {noreply, State, State#state.heartbeat_timeout};
handle_info(Info, State) ->
    log:i("Unknown Info: ~p.~n", [Info]),
    {noreply, State, State#state.heartbeat_timeout}.


terminate(Reason, #state{user_id = UserId} = State) ->
    ok = delete_useless_token(State#state.device_list),
    log:i("Client ~p terminate with reason: ~p~n", [self(), Reason]),
    ok = offline:store(UserId, State#state.msg_cache),
    session:unregister(UserId).
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

send_msg_cache([Device|T]) ->
    ok = loop_send_msg(Device#device.socket, Device#device.msg_cache),
    send_msg_cache(T);
send_msg_cache([]) ->
    ok.


loop_send_msg(Socket, [Message|T]) ->
    ok = gen_tcp:send(Socket, Message#message.bin),
    loop_send_msg(Socket, T);
loop_send_msg(_, []) ->
    ok.


merge_msg_cache(State, OldMsgCache) ->
    {ok, NewDeviceList} = merge_msg_cache(State#state.device_list,
                                          OldMsgCache,
                                          []),
    NewMsgCache = State#state.msg_cache ++ OldMsgCache,
    NewState = State#state{device_list = NewDeviceList,
                           msg_cache = NewMsgCache},
    {ok, NewState}.
merge_msg_cache([Device|T], OldMsgCache, NewDeviceList) ->
    NewDeviceMsgCache = Device#device.msg_cache ++ OldMsgCache,
    NewDevice = Device#device{msg_cache = NewDeviceMsgCache},
    ok = loop_send_msg(NewDevice#device.socket, NewDevice#device.msg_cache),
    merge_msg_cache(T, OldMsgCache, [NewDevice|NewDeviceList]);
merge_msg_cache([], _, NewDeviceList) ->
    {ok, NewDeviceList}.


send_msg_2_single_device(#device{socket = Socket} = Device, Message, State) ->
    ok = gen_tcp:send(Socket, Message#message.bin),
    NewDeviceMsgCache = [Message|Device#device.msg_cache],
    NewDevice = Device#device{msg_cache = NewDeviceMsgCache},
    NewDeviceList = lists:keystore(Socket,
                                   #device.socket,
                                   State#state.device_list,
                                   NewDevice),
    NewState = State#state{device_list = NewDeviceList},
    {ok, NewState}.


send_msg_2_multiple_device([Device|T], Message, State, Mode) ->
    {ok, NewState} = send_msg_2_single_device(Device, Message, State),
    send_msg_2_multiple_device(T, Message, NewState, Mode);
send_msg_2_multiple_device([], Message, State, save) ->
    NewMsgCache = [Message|State#state.msg_cache],
    NewState = State#state{msg_cache = NewMsgCache},
    {ok, NewState};
send_msg_2_multiple_device([], _, State, ignore) ->
    {ok, State}.


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


delete_useless_token([Device|T]) ->
    {ok, TokenKey} = redis:key({token, Device#device.token}),
    {ok, _} = redis:q([<<"DEL">>, TokenKey]),
    delete_useless_token(T);
delete_useless_token([]) ->
    ok.


% request
process_packet([{<<"r">>, Attrs}|T], Socket, State) ->
    {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
    log:i("Got r id=~p~n", [MsgId]),
    {value, Device, OtherDeivces} = lists:keytake(Socket,
                                                  #device.socket,
                                                  State#state.device_list),
    Type = lists:keyfind(<<"t">>, 1, Attrs),
    {RR, NewStateTemp} = process_request(Type, Attrs, MsgId, OtherDeivces, State),
    {ok, RRBin} = toml:term_2_binary(RR),
    RRMessage = #message{id = MsgId, bin = RRBin},
    {ok, NewState} = send_msg_2_single_device(Device, RRMessage, NewStateTemp),
    process_packet(T, Socket, NewState);
% message
process_packet([{<<"m">>, Attrs}|T], Socket, State) ->
    {ok, Message, NewState} = process_message(Socket, State, {<<"m">>, Attrs}),
    case lists:keyfind(<<"to">>, 1, Attrs) of
        {<<"to">>, ToUserId} ->
            ok = send_msg_2_single_user(ToUserId, Message);
        _ ->
            ignore
    end,
    process_packet(T, Socket, NewState);
% group message
process_packet([{<<"gm">>, Attrs}|T], Socket, State) ->
    {ok, Message, NewState} = process_message(Socket, State, {<<"gm">>, Attrs}),
    case lists:keyfind(<<"group">>, 1, Attrs) of
        {<<"group">>, GroupId} ->
            UserId = State#state.user_id,
            {ok, UserIdList} = group_members:find({group_id, GroupId}),
            ok = send_msg_2_multiple_user(UserIdList, UserId, Message);
        _ ->
            ignore
    end,
    process_packet(T, Socket, NewState);
% ack
process_packet([{<<"a">>, Attrs}|T], Socket, State) ->
    {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
    NewMsgCache = lists:keydelete(MsgId,
                                  #message.id,
                                  State#state.msg_cache),

    Device = lists:keyfind(Socket,
                           #device.socket,
                           State#state.device_list),
    NewDeviceMsgCache = lists:keydelete(MsgId,
                                        #message.id,
                                        Device#device.msg_cache),
    NewDevice = Device#device{msg_cache = NewDeviceMsgCache},
    NewDeviceList = lists:keystore(Socket,
                                   #device.socket,
                                   State#state.device_list,
                                   NewDevice),

    NewState = State#state{device_list = NewDeviceList,
                           msg_cache = NewMsgCache},
    process_packet(T, Socket, NewState);
process_packet([], _, NewState) ->
    {ok, NewState}.


process_request({<<"t">>, <<"add_contact">>}, Attrs, MsgId, OtherDeivces, State) ->
    {<<"to">>, ToUserId} = lists:keyfind(<<"to">>, 1, Attrs),
    UserId = State#state.user_id,
    {<<"message">>, AddContactMessage} = lists:keyfind(<<"message">>, 1, Attrs),
    case pre_contacts:create(UserId, ToUserId, AddContactMessage) of
        {ok, 0} ->
            NewAttrs = add_ts_from(Attrs, UserId),
            {ok, RequestBin} = toml:term_2_binary({<<"r">>, NewAttrs}),
            Message = #message{id = MsgId, bin = RequestBin},
            {ok, NewState} = send_msg_2_multiple_device(OtherDeivces,
                                                        Message,
                                                        State,
                                                        ignore),
            ok = send_msg_2_single_user(ToUserId, Message),
            RR = {<<"rr">>, [{<<"id">>, MsgId}, {<<"status">>, 0}]},
            {RR, NewState};
        {ok, 1} ->
            RR = {<<"rr">>, [{<<"id">>, MsgId},
                             {<<"status">>, 1},
                             {<<"r">>, <<"Contact exists">>}]},
            {RR, State};
        {ok, 2} ->
            RR = {<<"rr">>, [{<<"id">>, MsgId},
                             {<<"status">>, 1},
                             {<<"r">>, <<"Waiting for accept">>}]},
            {RR, State};
        {ok, _} ->
            RR = {<<"rr">>, [{<<"id">>, MsgId},
                             {<<"status">>, 1},
                             {<<"r">>, <<"Unkonw Error">>}]},
            {RR, State}
    end;
process_request({<<"t">>, <<"accept_contact">>}, Attrs, MsgId, OtherDeivces, State) ->
    {<<"to">>, AUserId} = lists:keyfind(<<"to">>, 1, Attrs),
    BUserId = State#state.user_id,
    ok = contacts:create(AUserId, BUserId),

    NewAttrs = add_ts_from(Attrs, BUserId),
    {ok, RequestBin} = toml:term_2_binary({<<"r">>, NewAttrs}),
    Message = #message{id = MsgId, bin = RequestBin},
    {ok, NewState} = send_msg_2_multiple_device(OtherDeivces,
                                                Message,
                                                State,
                                                ignore),
    ok = send_msg_2_single_user(AUserId, Message),

    RR = {<<"rr">>, [{<<"id">>, MsgId}, 
                     {<<"status">>, 0}]},
    {RR, NewState};
process_request({<<"t">>, <<"delete_contact">>}, Attrs, MsgId, OtherDeivces, State) ->
    {<<"to">>, ToUserId} = lists:keyfind(<<"to">>, 1, Attrs),
    UserId = State#state.user_id,
    ok = contacts:delete(UserId, ToUserId),

    NewAttrs = add_ts_from(Attrs, UserId),
    {ok, RequestBin} = toml:term_2_binary({<<"r">>, NewAttrs}),
    Message = #message{id = MsgId, bin = RequestBin},
    {ok, NewState} = send_msg_2_multiple_device(OtherDeivces,
                                                Message,
                                                State,
                                                ignore),
    ok = send_msg_2_single_user(ToUserId, Message),

    RR = {<<"rr">>, [{<<"id">>, MsgId}, 
                     {<<"status">>, 0}]},
    {RR, NewState};
process_request({<<"t">>, <<"create_group">>}, Attrs, MsgId, OtherDeivces, State) ->
    {<<"name">>, GroupName} = lists:keyfind(<<"name">>, 1, Attrs),
    {<<"members">>, Members} = lists:keyfind(<<"members">>, 1, Attrs),
    UserId = State#state.user_id,

    {ok, GroupId, Key} = groups:create(GroupName, UserId, Members),

    Ts = {<<"ts">>, utility:timestamp()},
    NewAttrs = lists:keystore(<<"ts">>, 1, Attrs, Ts),
    {ok, RequestBin} = toml:term_2_binary({<<"r">>, NewAttrs}),
    Message = #message{id = MsgId, bin = RequestBin},
    {ok, NewState} = send_msg_2_multiple_device(OtherDeivces,
                                                Message,
                                                State,
                                                ignore),
    ok = send_msg_2_multiple_user(Members, UserId, Message),

    RR = {<<"rr">>, [{<<"id">>, MsgId}, 
                     {<<"status">>, 0},
                     {<<"group">>, [{<<"id">>, GroupId},
                                    {<<"key">>, Key}]}]},
    {RR, NewState};
process_request({<<"t">>, <<"delete_group">>}, Attrs, MsgId, OtherDeivces, State) ->
    {<<"group_id">>, GroupId} = lists:keyfind(<<"group_id">>, 1, Attrs),
    UserId = State#state.user_id,
    {ok, Members} = group_members:find({group_id, GroupId}),
    ok = groups:delete(GroupId, UserId),

    Ts = {<<"ts">>, utility:timestamp()},
    NewAttrs = lists:keystore(<<"ts">>, 1, Attrs, Ts),
    {ok, RequestBin} = toml:term_2_binary({<<"r">>, NewAttrs}),
    Message = #message{id = MsgId, bin = RequestBin},
    {ok, NewState} = send_msg_2_multiple_device(OtherDeivces,
                                                Message,
                                                State,
                                                ignore),
    ok = send_msg_2_multiple_user(Members, UserId, Message),

    RR = {<<"rr">>, [{<<"id">>, MsgId}, 
                     {<<"status">>, 0}]},
    {RR, NewState};
process_request({<<"t">>, <<"create_group_member">>}, Attrs, MsgId, OtherDeivces, State) ->
    {<<"group_id">>, GroupId} = lists:keyfind(<<"group_id">>, 1, Attrs),
    {<<"key">>, Key} = lists:keyfind(<<"key">>, 1, Attrs),
    UserId = State#state.user_id,
    {ok, Members} = group_members:find({group_id, GroupId}),
    ok = group_members:create(GroupId, Key, UserId),

    Ts = {<<"ts">>, utility:timestamp()},
    NewAttrs = lists:keystore(<<"ts">>, 1, Attrs, Ts),
    {ok, RequestBin} = toml:term_2_binary({<<"r">>, NewAttrs}),
    Message = #message{id = MsgId, bin = RequestBin},
    {ok, NewState} = send_msg_2_multiple_device(OtherDeivces,
                                                Message,
                                                State,
                                                ignore),
    ok = send_msg_2_multiple_user(Members, UserId, Message),

    RR = {<<"rr">>, [{<<"id">>, MsgId}, 
                     {<<"status">>, 0}]},
    {RR, NewState};
process_request({<<"t">>, <<"delete_group_member">>}, Attrs, MsgId, OtherDeivces, State) ->
    {<<"group_id">>, GroupId} = lists:keyfind(<<"group_id">>, 1, Attrs),
    {ok, Members} = group_members:find({group_id, GroupId}),
    UserId = State#state.user_id,
    ok = group_members:delete(GroupId, UserId),

    Ts = {<<"ts">>, utility:timestamp()},
    NewAttrs = lists:keystore(<<"ts">>, 1, Attrs, Ts),
    {ok, RequestBin} = toml:term_2_binary({<<"r">>, NewAttrs}),
    Message = #message{id = MsgId, bin = RequestBin},
    {ok, NewState} = send_msg_2_multiple_device(OtherDeivces,
                                                Message,
                                                State,
                                                ignore),
    ok = send_msg_2_multiple_user(Members, UserId, Message),

    RR = {<<"rr">>, [{<<"id">>, MsgId}, 
                     {<<"status">>, 0}]},
    {RR, NewState};
process_request(_, _, MsgId, _, State) ->
    RR = {<<"rr">>,
          [{<<"id">>, MsgId},
           {<<"s">>, 1},
           {<<"r">>, <<"Unknown request">>}]},
    {RR, State}.


process_message(Socket, State, {Type, Attrs}) ->
    {value, Device, OtherDeivces} = lists:keytake(Socket,
                                                  #device.socket,
                                                  State#state.device_list),

    {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
    log:i("Got msg id=~p~n", [MsgId]),
    % Ack = {<<"a">>,
    %        [{<<"id">>, MsgId}]},
    % {ok, AckBin} = toml:term_2_binary(Ack),
    AckBin = <<"[[a]] id = \"", MsgId/binary, "\"">>,
    AckMessage = #message{id = MsgId, bin = AckBin},
    {ok, NewStateTemp} = send_msg_2_single_device(Device, AckMessage, State),

    NewAttrs = add_ts_from(Attrs, State#state.user_id),
    NewToml = {Type, NewAttrs},
    {ok, NewTomlBin} = toml:term_2_binary(NewToml),
    Message = #message{id = MsgId, bin = NewTomlBin},
    {ok, NewState} = send_msg_2_multiple_device(OtherDeivces,
                                                Message,
                                                NewStateTemp,
                                                ignore),
    {ok, Message, NewState}.


add_ts_from(Attrs, UserId) ->
    Ts = {<<"ts">>, utility:timestamp()},
    AttrsWithTs = lists:keystore(<<"ts">>, 1, Attrs, Ts),
    From = {<<"from">>, UserId},
    lists:keystore(<<"from">>, 1, AttrsWithTs, From).


send_msg_2_single_user(UserId, Message) ->
    case session:find(UserId) of
        offline ->
            ok = offline:store(UserId, [Message]),
            log:i("offline msg: ~p~n", [Message]);
        {ok, ToPid} ->
            ToPid ! Message
    end,
    ok.


send_msg_2_multiple_user([BeIgnoredUserId|T], BeIgnoredUserId, Message) ->
    send_msg_2_multiple_user(T, BeIgnoredUserId, Message);
send_msg_2_multiple_user([UserId|T], BeIgnoredUserId, Message) ->
    ok = send_msg_2_single_user(UserId, Message),
    send_msg_2_multiple_user(T, BeIgnoredUserId, Message);
send_msg_2_multiple_user([], _, _) ->
    ok.