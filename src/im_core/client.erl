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

-record(state, {socket,
                heartbeat_timeout,
                user,
                msg_cache}). %% [#message{},...]

-include("user.hrl").
-include("message.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

start_link(Socket, Message, User) ->
    gen_server:start_link(?MODULE, [Socket, Message, User], []).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Socket, Message, User]) ->
    session:register(User, self()),
    ok = send_tcp(Socket, Message),
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

handle_info({replace_socket, NewSocket, Message, User}, #state{socket = Socket} = State) ->
    gen_tcp:close(Socket),
    ok = clean_mailbox(Socket),
    ok = send_tcp(NewSocket, Message),
    setopts(NewSocket),
    NewState = State#state{socket = NewSocket, user = User},
    ok = send_msg_cache(NewSocket, State#state.msg_cache),
    {noreply, NewState, NewState#state.heartbeat_timeout};
handle_info({tcp, Socket, Data}, #state{socket = Socket} = State) ->
    {ok, Toml} = toml:binary_2_term(Data),
    {ok, NewState} = process_packet(Toml, State),
    {noreply, NewState, NewState#state.heartbeat_timeout};
% tcp connection change to passive
handle_info({tcp_passive, Socket}, #state{socket = Socket} = State) ->
    setopts(Socket),
    {noreply, State, State#state.heartbeat_timeout};
% connection closed
handle_info({tcp_closed, _Socket}, State) ->
    {noreply, State, State#state.heartbeat_timeout};



%% ===================================================================
%% business receiver
%% ===================================================================

handle_info(#message{} = Message, State) ->
    ok = send_tcp(State#state.socket, Message),
    NewMsgCache = [Message|State#state.msg_cache],
    NewState = State#state{msg_cache = NewMsgCache},
    {noreply, NewState, NewState#state.heartbeat_timeout};



%% ===================================================================
%% client logic functions
%% ===================================================================

handle_info({replaced_by, NewPid}, State) ->
    NewPid ! {msg_cache, State#state.msg_cache},
    {stop, {replaced_by, NewPid}, State};
handle_info({msg_cache, OriginalMsgCache}, #state{msg_cache = MsgCache} = State) ->
    NewState = State#state{msg_cache = MsgCache ++ OriginalMsgCache},
    ok = send_msg_cache(State#state.socket, MsgCache),
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
    log:i("Client ~p terminate with reason: ~p~n", [self(), Reason]),
    offline:store(User#user.id, MsgCache),
    session:unregister(User).
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

send_tcp(Socket, Message) ->
    {ok, MsgBin} = toml:term_2_binary(Message#message.toml),
    gen_tcp:send(Socket, MsgBin),
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


send_msg_cache(Socket, [H|T]) ->
    send_tcp(Socket, H),
    send_msg_cache(Socket, T);
send_msg_cache(_, []) ->
    ok.


% request
process_packet([{<<"r">>, Attrs}|T], State) ->
    {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
    log:i("Got r id=~p~n", [MsgId]),
    case lists:keyfind(<<"t">>, 1, Attrs) of
        {<<"t">>, <<"add_contact">>} ->
            case lists:keyfind(<<"to">>, 1, Attrs) of
                {<<"to">>, ToUserId} ->
                    UserId = State#state.user#user.id,
                    {<<"message">>, AddContactMessage} = lists:keyfind(<<"message">>, 1, Attrs),
                    RR = case pre_contacts:create(UserId, ToUserId, AddContactMessage) of
                        {ok, 0} ->
                            NewAttrs = add_ts_from(Attrs, UserId),
                            Message = #message{id = MsgId, toml = {<<"r">>, NewAttrs}},
                            send_msg_2_single_user(ToUserId, Message),
                            {<<"rr">>, [{<<"id">>, MsgId}, {<<"status">>, 0}]};
                        {ok, 1} ->
                            {<<"rr">>, [{<<"id">>, MsgId},
                                        {<<"status">>, 1},
                                        {<<"r">>, <<"Contact exists">>}]};
                        {ok, 2} ->
                            {<<"rr">>, [{<<"id">>, MsgId},
                                        {<<"status">>, 2},
                                        {<<"r">>, <<"Waiting for accept">>}]};
                        {ok, _} ->
                            {<<"rr">>, [{<<"id">>, MsgId},
                                        {<<"status">>, 3},
                                        {<<"r">>, <<"Unkonw Error">>}]}
                    end,
                    {ok, NewState} = send_rr(MsgId, RR, State);
                _ ->
                    NewState = State
            end,
            process_packet(T, NewState);
        {<<"t">>, <<"accept_contact">>} ->
            case lists:keyfind(<<"to">>, 1, Attrs) of
                {<<"to">>, ToUserId} ->
                    UserId = State#state.user#user.id,
                    {ok, [UserVersion, ToUserVersion]} = contacts:create(UserId, ToUserId),

                    NewAttrs = [{<<"contact_version">>, ToUserVersion}|add_ts_from(Attrs, UserId)],
                    Message = #message{id = MsgId, toml = {<<"r">>, NewAttrs}},
                    send_msg_2_single_user(ToUserId, Message),

                    RR = {<<"rr">>, [{<<"id">>, MsgId}, 
                                     {<<"status">>, 0},
                                     {<<"contact_version">>, UserVersion}]},
                    {ok, NewState} = send_rr(MsgId, RR, State);
                _ ->
                    NewState = State
            end,
            process_packet(T, NewState);
        _ ->
            RR = {<<"rr">>,
                  [{<<"id">>, MsgId},
                   {<<"s">>, 1},
                   {<<"r">>, <<"Unknown request">>}]},
            {ok, NewState} = send_rr(MsgId, RR, State),
            process_packet(T, NewState)
    end;
% message
process_packet([{<<"m">>, Attrs} = Msg|T], State) ->
    {ok, Message, NewState} = process_message(State, Msg),
    case lists:keyfind(<<"to">>, 1, Attrs) of
        {<<"to">>, ToUserId} ->
            send_msg_2_single_user(ToUserId, Message);
        _ ->
            ignore
    end,
    process_packet(T, NewState);
% group message
process_packet([{<<"gm">>, Attrs} = Msg|T], State) ->
    {ok, Message, NewState} = process_message(State, Msg),
    case lists:keyfind(<<"group">>, 1, Attrs) of
        {<<"group">>, GroupId} ->
            {ok, UserIdList} = groups:get_user_id_list(GroupId),
            ok = send_msg_2_multiple_users(UserIdList, Message);
        _ ->
            ignore
    end,
    process_packet(T, NewState);
% ack
process_packet([{<<"a">>, Attrs}|T], State) ->
    {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
    NewMsgCache = lists:keydelete(MsgId, 2, State#state.msg_cache),
    process_packet(T, State#state{msg_cache = NewMsgCache});
process_packet([], NewState) ->
    {ok, NewState}.


process_message(State, {Type, Attrs}) ->
    {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
    log:i("Got msg id=~p~n", [MsgId]),
    Ack = {<<"a">>,
           [{<<"id">>, MsgId}]},
    AckMessage = #message{id = MsgId, toml = Ack},
    NewMsgCache = [AckMessage|State#state.msg_cache],
    ok = send_tcp(State#state.socket, AckMessage),

    NewAttrs = add_ts_from(Attrs, State#state.user#user.id),

    NewToml = {Type, NewAttrs},
    Message = #message{id = MsgId, toml = NewToml},
    {ok, Message, State#state{msg_cache = NewMsgCache}}.


add_ts_from(Attrs, UserId) ->
    Ts = {<<"ts">>, utility:timestamp()},
    AttrsWithTs = lists:keystore(<<"ts">>, 1, Attrs, Ts),
    From = {<<"from">>, UserId},
    lists:keystore(<<"from">>, 1, AttrsWithTs, From).


send_msg_2_single_user(UserId, Message) ->
    case session:get_pid_list(UserId) of
        offline ->
            offline:store(UserId, [Message]),
            log:i("offline msg: ~p~n", [Message]),
            ok;
        ToPidList ->
            send_msg_to_pid(ToPidList, Message)
    end.


send_msg_2_multiple_users([H|T], Message) ->
    send_msg_2_single_user(H, Message),
    send_msg_2_multiple_users(T, Message);
send_msg_2_multiple_users([], _) ->
    ok.


send_msg_to_pid([H|T], Message) ->
    H ! Message,
    send_msg_to_pid(T, Message);
send_msg_to_pid([], _) ->
    ok.


send_rr(MsgId, RRToml, State) ->
    Message = #message{id = MsgId, toml = RRToml},
    ok = send_tcp(State#state.socket, Message),
    NewMsgCache = [Message|State#state.msg_cache],
    {ok, State#state{msg_cache = NewMsgCache}}.