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
    NewState = case lists:keytake(Device#device.name,
                                  #device.name, DeviceList) of
        {value, #device{socket = OldSocket} = OldDevice, OtherDeivces} ->
            gen_tcp:close(OldSocket),
            ok = clean_mailbox(OldSocket),
            ok = delete_useless_token([OldDevice]),

            OldDeviceMsgCache = OldDevice#device.msg_cache,
            ok = send_msg_cache(Socket, [Message|OldDeviceMsgCache]),

            NewDevice = Device#device{msg_cache = OldDeviceMsgCache},
            State#state{device_list = [NewDevice|OtherDeivces]};
        false ->
            ok = gen_tcp:send(Socket, Message#message.bin),
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
    ok = send_msg_2_multiple_device(State#state.device_list, Message),
    NewMsgCache = [Message|State#state.msg_cache],
    NewState = State#state{msg_cache = NewMsgCache},
    {noreply, NewState, NewState#state.heartbeat_timeout};



%% ===================================================================
%% client logic functions
%% ===================================================================

handle_info({replaced_by, NewPid}, State) ->
    ok = delete_useless_token(State#state.device_list),
    NewPid ! {msg_cache, State#state.msg_cache},
    {stop, {replaced_by, NewPid}, State};
handle_info({msg_cache, OriginalMsgCache},
            #state{msg_cache = MsgCache} = State) ->
    NewState = case OriginalMsgCache of
        [] ->
            State;
        _ ->
            ok = send_msg_2_multiple1_device(State#state.device_list,
                                            OriginalMsgCache),
            State#state{msg_cache = MsgCache ++ OriginalMsgCache}
    end,
    {noreply, NewState, NewState#state.heartbeat_timeout};
handle_info(timeout, State) ->
    ok = offline:store(State#state.user_id, State#state.msg_cache),
    proc_lib:hibernate(gen_server, enter_loop, [?MODULE, [], State]),
    {noreply, State, State#state.heartbeat_timeout};
handle_info(Info, State) ->
    log:i("Unknown Info: ~p.~n", [Info]),
    {noreply, State, State#state.heartbeat_timeout}.


terminate(Reason, #state{msg_cache = MsgCache, user_id = UserId}) ->
    log:i("Client ~p terminate with reason: ~p~n", [self(), Reason]),
    ok = offline:store(UserId, MsgCache),
    session:unregister(UserId).
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

send_msg_cache(Socket, [Message|T]) ->
    ok = gen_tcp:send(Socket, Message#message.bin),
    send_msg_cache(Socket, T);
send_msg_cache(_, []) ->
    ok.


send_msg_2_multiple_device([Device|T], Message) ->
    ok = gen_tcp:send(Device#device.socket, Message#message.bin),
    send_msg_2_multiple_device(T, Message);
send_msg_2_multiple_device([], _) ->
    ok.


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
    case lists:keyfind(<<"t">>, 1, Attrs) of
        % {<<"t">>, <<"add_contact">>} ->
        %     case lists:keyfind(<<"to">>, 1, Attrs) of
        %         {<<"to">>, ToUserId} ->
        %             UserId = State#state.user#user.id,
        %             {<<"message">>, AddContactMessage} = lists:keyfind(<<"message">>, 1, Attrs),
        %             RR = case pre_contacts:create(UserId, ToUserId, AddContactMessage) of
        %                 {ok, 0} ->
        %                     NewAttrs = add_ts_from(Attrs, UserId),
        %                     Message = #message{id = MsgId, toml = {<<"r">>, NewAttrs}},
        %                     send_msg_2_single_user(ToUserId, Message),
        %                     {<<"rr">>, [{<<"id">>, MsgId}, {<<"status">>, 0}]};
        %                 {ok, 1} ->
        %                     {<<"rr">>, [{<<"id">>, MsgId},
        %                                 {<<"status">>, 1},
        %                                 {<<"r">>, <<"Contact exists">>}]};
        %                 {ok, 2} ->
        %                     {<<"rr">>, [{<<"id">>, MsgId},
        %                                 {<<"status">>, 2},
        %                                 {<<"r">>, <<"Waiting for accept">>}]};
        %                 {ok, _} ->
        %                     {<<"rr">>, [{<<"id">>, MsgId},
        %                                 {<<"status">>, 3},
        %                                 {<<"r">>, <<"Unkonw Error">>}]}
        %             end,
        %             {ok, NewState} = send_rr(MsgId, RR, State);
        %         _ ->
        %             NewState = State
        %     end,
        %     process_packet(T, NewState);
        % {<<"t">>, <<"accept_contact">>} ->
        %     case lists:keyfind(<<"to">>, 1, Attrs) of
        %         {<<"to">>, ToUserId} ->
        %             UserId = State#state.user#user.id,
        %             {ok, [UserVersion, ToUserVersion]} = contacts:create(UserId, ToUserId),

        %             NewAttrs = [{<<"contact_version">>, ToUserVersion}|add_ts_from(Attrs, UserId)],
        %             Message = #message{id = MsgId, toml = {<<"r">>, NewAttrs}},
        %             send_msg_2_single_user(ToUserId, Message),

        %             RR = {<<"rr">>, [{<<"id">>, MsgId}, 
        %                              {<<"status">>, 0},
        %                              {<<"contact_version">>, UserVersion}]},
        %             {ok, NewState} = send_rr(MsgId, RR, State);
        %         _ ->
        %             NewState = State
        %     end,
        %     process_packet(T, NewState);
        % {<<"t">>, <<"delete_contact">>} ->
        %     case lists:keyfind(<<"to">>, 1, Attrs) of
        %         {<<"to">>, ToUserId} ->
        %             UserId = State#state.user#user.id,
        %             {ok, [UserVersion, ToUserVersion]} = contacts:delete(UserId, ToUserId),

        %             NewAttrs = [{<<"contact_version">>, ToUserVersion}|add_ts_from(Attrs, UserId)],
        %             Message = #message{id = MsgId, toml = {<<"r">>, NewAttrs}},
        %             send_msg_2_single_user(ToUserId, Message),

        %             RR = {<<"rr">>, [{<<"id">>, MsgId}, 
        %                              {<<"status">>, 0},
        %                              {<<"contact_version">>, UserVersion}]},
        %             {ok, NewState} = send_rr(MsgId, RR, State);
        %         _ ->
        %             NewState = State
        %     end,
        %     process_packet(T, NewState);
        % {<<"t">>, <<"create_group">>} ->
        %     case lists:keyfind(<<"name">>, 1, Attrs) of
        %         {<<"name">>, GroupName} ->
        %             case lists:keyfind(<<"members">>, 1, Attrs) of
        %                 {<<"members">>, Members} ->
        %                     UserId = State#state.user#user.id,

        %                     {ok, GroupId, Key} = groups:create(GroupName, UserId, Members),

        %                     Ts = {<<"ts">>, utility:timestamp()},
        %                     NewAttrs = lists:keystore(<<"ts">>, 1, Attrs, Ts),
        %                     Message = #message{id = MsgId, toml = {<<"r">>, NewAttrs}},
        %                     ok = send_msg_2_multiple_user(Members, Message),

        %                     RR = {<<"rr">>, [{<<"id">>, MsgId}, 
        %                                      {<<"status">>, 0},
        %                                      {<<"group">>, [{<<"id">>, GroupId},
        %                                                     {<<"key">>, Key}]}]},
        %                     {ok, NewState} = send_rr(MsgId, RR, State);
        %                 _ ->
        %                     NewState = State
        %             end;
        %         _ ->
        %             NewState = State
        %     end,
        %     process_packet(T, NewState);
        % {<<"t">>, <<"delete_group">>} ->
        %     case lists:keyfind(<<"group_id">>, 1, Attrs) of
        %         {<<"group_id">>, GroupId} ->
        %             UserId = State#state.user#user.id,
        %             {ok, Members} = group_members:find({group_id, GroupId}),
        %             ok = groups:delete(GroupId, UserId),

        %             Ts = {<<"ts">>, utility:timestamp()},
        %             NewAttrs = lists:keystore(<<"ts">>, 1, Attrs, Ts),
        %             Message = #message{id = MsgId, toml = {<<"r">>, NewAttrs}},
        %             ok = send_msg_2_multiple_user(Members, Message),

        %             RR = {<<"rr">>, [{<<"id">>, MsgId}, 
        %                              {<<"status">>, 0}]},
        %             {ok, NewState} = send_rr(MsgId, RR, State);
        %         _ ->
        %             NewState = State
        %     end,
        %     process_packet(T, NewState);
        % {<<"t">>, <<"create_group_member">>} ->
        %     case lists:keyfind(<<"group_id">>, 1, Attrs) of
        %         {<<"group_id">>, GroupId} ->
        %             case lists:keyfind(<<"")
        %         _ ->
        %             NewState = State
        %     end,
        %     process_packet(T, NewState);
        _ ->
            % RR = {<<"rr">>,
            %       [{<<"id">>, MsgId},
            %        {<<"s">>, 1},
            %        {<<"r">>, <<"Unknown request">>}]},
            % {ok, NewState} = send_rr(MsgId, RR, State),
            process_packet(T, Socket, State)
    end;
% message
process_packet([{<<"m">>, Attrs} = Toml|T], Socket, State) ->
    {ok, Message, NewState} = process_message(Socket, State, Toml),
    case lists:keyfind(<<"to">>, 1, Attrs) of
        {<<"to">>, ToUserId} ->
            ok = send_msg_2_single_user(ToUserId, Message);
        _ ->
            ignore
    end,
    process_packet(T, Socket, NewState);
% group message
process_packet([{<<"gm">>, Attrs} = Toml|T], Socket, State) ->
    {ok, Message, NewState} = process_message(Socket, State, Toml),
    case lists:keyfind(<<"group">>, 1, Attrs) of
        {<<"group">>, GroupId} ->
            {ok, UserIdList} = group_members:find({group_id, GroupId}),
            ok = send_msg_2_multiple_user(UserIdList, Message);
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


process_message(Socket, State, {Type, Attrs}) ->
    {value, Device, OtherDeivces} = lists:keytake(Socket, #device.socket, State#state.device_list),

    {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
    log:i("Got msg id=~p~n", [MsgId]),
    Ack = {<<"a">>,
           [{<<"id">>, MsgId}]},
    {ok, AckBin} = toml:term_2_binary(Ack),
    AckMessage = #message{id = MsgId, bin = AckBin},
    ok = gen_tcp:send(Device#device.socket, AckBin),
    NewMsgCache = [AckMessage|State#state.msg_cache],

    NewAttrs = add_ts_from(Attrs, State#state.user_id),
    NewToml = {Type, NewAttrs},
    {ok, NewTomlBin} = toml:term_2_binary(NewToml),
    Message = #message{id = MsgId, bin = NewTomlBin},
    ok = send_msg_2_multiple_device(OtherDeivces, Message),
    {ok, Message, State#state{msg_cache = NewMsgCache}}.


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


send_msg_2_multiple_user([UserId|T], Message) ->
    send_msg_2_single_user(UserId, Message),
    send_msg_2_multiple_user(T, Message);
send_msg_2_multiple_user([], _) ->
    ok.


% send_rr(MsgId, RRToml, State) ->
%     Message = #message{id = MsgId, toml = RRToml},
%     {ok, MessageBin} = toml:term_2_binary(RRToml),
%     ok = gen_tcp:send(Device#device.socket, MessageBin),
%     NewMsgCache = [Message|State#state.msg_cache],
%     {ok, State#state{msg_cache = NewMsgCache}}.