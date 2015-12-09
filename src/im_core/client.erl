%% ===================================================================
%% Author xiaotie
%% 2015-8-2
%% server for client side
%% ===================================================================

-module(client).

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

init([Message, UserId, #device{ssl_socket = SslSocket} = Device]) ->
    ok = session:register(UserId, self()),
    ok = ssl:send(Device#device.ssl_socket, Message#message.bin),
    State = #state{heartbeat_timeout = env:get(heartbeat_timeout),
                   user_id = UserId,
                   device_list = [Device],
                   msg_cache = []},
    ok = setopts(SslSocket),
    {ok, State, State#state.heartbeat_timeout}.


handle_call(_Request, _From, State) ->
    {reply, nomatch, State, State#state.heartbeat_timeout}.
handle_cast(_Msg, State) ->
    {noreply, State, State#state.heartbeat_timeout}.



%% ===================================================================
%% socket
%% ===================================================================

handle_info({replace_socket, Message, #device{ssl_socket = SslSocket} = Device},
            #state{device_list = DeviceList} = State) ->
    ok = ssl:send(SslSocket, Message#message.bin),

    NewState = case lists:keytake(Device#device.name,
                                  #device.name, DeviceList) of
        {value, #device{ssl_socket = OldSslSocket} = OldDevice, OtherDeivces} ->
            ok = ssl:close(OldSslSocket),
            ok = clean_mailbox(OldSslSocket),
            ok = delete_useless_token([OldDevice]),

            OldDeviceMsgCache = OldDevice#device.msg_cache,
            NewDevice = Device#device{msg_cache = OldDeviceMsgCache},
            ok = send_msg_cache([NewDevice]),

            State#state{device_list = [NewDevice|OtherDeivces]};
        false ->
            State#state{device_list = [Device|DeviceList]}
    end,
    setopts(SslSocket),
    {noreply, NewState, NewState#state.heartbeat_timeout};
handle_info({ssl, _, Bin}, State) ->
    {ok, Toml} = toml:binary_2_term(Bin),
    {ok, NewState} = process_packet(Toml, State),
    {noreply, NewState, NewState#state.heartbeat_timeout};
% connection closed
handle_info({ssl_closed, _SslSocket}, State) ->
    {noreply, State, State#state.heartbeat_timeout};



%% ===================================================================
%% business receiver
%% ===================================================================

handle_info({middleman, Toml}, State) ->
    {ok, NewState} = process_packet(Toml, State),
    {noreply, NewState, NewState#state.heartbeat_timeout};
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
    log:i("[IM] Client got an unknown info: ~p.~n", [Info]),
    {noreply, State, State#state.heartbeat_timeout}.


terminate(Reason, #state{user_id = UserId} = State) ->
    ok = delete_useless_token(State#state.device_list),
    log:i("[IM] Client ~p terminate with reason: ~p~n", [self(), Reason]),
    ok = offline:store(UserId, State#state.msg_cache),
    session:unregister(UserId).
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

send_msg_cache([Device|T]) ->
    ok = loop_send_msg(Device#device.ssl_socket, Device#device.msg_cache),
    send_msg_cache(T);
send_msg_cache([]) ->
    ok.


loop_send_msg(SslSocket, [Message|T]) ->
    ok = ssl:send(SslSocket, Message#message.bin),
    loop_send_msg(SslSocket, T);
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
    ok = loop_send_msg(NewDevice#device.ssl_socket, NewDevice#device.msg_cache),
    merge_msg_cache(T, OldMsgCache, [NewDevice|NewDeviceList]);
merge_msg_cache([], _, NewDeviceList) ->
    {ok, NewDeviceList}.


send_msg_2_single_device(#device{ssl_socket = SslSocket} = Device, Message, State) ->
    ok = ssl:send(SslSocket, Message#message.bin),
    NewDeviceMsgCache = [Message|Device#device.msg_cache],
    NewDevice = Device#device{msg_cache = NewDeviceMsgCache},
    NewDeviceList = lists:keystore(SslSocket,
                                   #device.ssl_socket,
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


setopts(SslSocket) ->
    ok = ssl:setopts(SslSocket, [{active, true}, {packet, 0}, binary]),
    ok.


clean_mailbox(OldSslSocket) ->
    receive
        {ssl, OldSslSocket, _} ->
            clean_mailbox(OldSslSocket);
        {ssl_closed, OldSslSocket} ->
            clean_mailbox(OldSslSocket)
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



% message
process_packet([{<<"m">>, Attrs}|T], State) ->
    {ok, Message, NewState} = process_message(State, {<<"m">>, Attrs}),
    case lists:keyfind(<<"to">>, 1, Attrs) of
        {<<"to">>, ToUserId} ->
            ok = router:route_to_single_user(ToUserId, Message);
        _ ->
            ignore
    end,
    process_packet(T, NewState);
% group message
process_packet([{<<"gm">>, Attrs}|T], State) ->
    {ok, Message, NewState} = process_message(State, {<<"gm">>, Attrs}),
    case lists:keyfind(<<"g_id">>, 1, Attrs) of
        {<<"g_id">>, GroupId} ->
            UserId = State#state.user_id,
            {ok, UserIdList} = group_members:find({group_id, GroupId}),
            ok = router:route_to_multiple_user(UserIdList, UserId, Message);
        _ ->
            ignore
    end,
    process_packet(T, NewState);
% ack
process_packet([{<<"a">>, Attrs}|T], State) ->
    {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
    {<<"d">>, DeviceName} = lists:keyfind(<<"d">>, 1, Attrs),
    NewMsgCache = lists:keydelete(MsgId,
                                  #message.id,
                                  State#state.msg_cache),

    Device = lists:keyfind(DeviceName,
                           #device.name,
                           State#state.device_list),
    NewDeviceMsgCache = lists:keydelete(MsgId,
                                        #message.id,
                                        Device#device.msg_cache),
    NewDevice = Device#device{msg_cache = NewDeviceMsgCache},
    NewDeviceList = lists:keystore(DeviceName,
                                   #device.name,
                                   State#state.device_list,
                                   NewDevice),

    NewState = State#state{device_list = NewDeviceList,
                           msg_cache = NewMsgCache},
    process_packet(T, NewState);
process_packet([], NewState) ->
    {ok, NewState}.


process_message(State, {Type, Attrs}) ->
    {<<"d">>, DeviceName} = lists:keyfind(<<"d">>, 1, Attrs),
    {value, Device, OtherDeivces} = lists:keytake(DeviceName,
                                                  #device.name,
                                                  State#state.device_list),

    {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
    log:i("[IM] Client got msg id=~p~n", [MsgId]),
    % Ack = {<<"a">>,
    %        [{<<"id">>, MsgId}]},
    % {ok, AckBin} = toml:term_2_binary(Ack),
    AckBin = <<"[[a]] id = \"", MsgId/binary, "\"">>,
    AckMessage = #message{id = MsgId, bin = AckBin},
    {ok, NewStateTemp} = send_msg_2_single_device(Device, AckMessage, State),

    Ts = {<<"ts">>, utility:timestamp()},
    AttrsWithTs = lists:keystore(<<"ts">>, 1, Attrs, Ts),
    From = {<<"from">>, State#state.user_id},
    NewAttrs = lists:keystore(<<"from">>, 1, AttrsWithTs, From),
    NewToml = {Type, NewAttrs},
    {ok, NewTomlBin} = toml:term_2_binary(NewToml),
    Message = #message{id = MsgId, bin = NewTomlBin},
    {ok, NewState} = send_msg_2_multiple_device(OtherDeivces,
                                                Message,
                                                NewStateTemp,
                                                ignore),
    {ok, Message, NewState}.