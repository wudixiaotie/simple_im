-module (client).

-behaviour(gen_server).

% APIs
-export([start_link/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record(state, {socket,
                heartbeat_timeout,
                user}).

-include("user.hrl").

%% ===================================================================
%% APIs
%% ===================================================================

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Socket]) ->
    State = #state{socket = Socket,
                   heartbeat_timeout = env:get(heartbeat_timeout)},
    setopts(State#state.socket),
    {ok, State, State#state.heartbeat_timeout}.
handle_call(_Request, _From, State) ->
    {reply, nomatch, State, State#state.heartbeat_timeout}.
handle_cast(_Msg, State) ->
    {noreply, State, State#state.heartbeat_timeout}.


%% ===================================================================
%% tcp receiver
%% ===================================================================

handle_info({tcp, Socket, Data}, #state{socket = Socket} = State) ->
    {ok, Toml} = etoml:parse(Data),
    {ok, NewState} = process_packet(Toml, State),
    {noreply, NewState, NewState#state.heartbeat_timeout};
% tcp connection change to passive
handle_info({tcp_passive, Swocket}, #state{socket = Swocket} = State) ->
    setopts(Swocket),
    {noreply, State, State#state.heartbeat_timeout};
% connection closed
handle_info({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};
handle_info(timeout, State) ->
    proc_lib:hibernate(gen_server, enter_loop, [?MODULE, [], State]),
    {noreply, State, State#state.heartbeat_timeout};


%% ===================================================================
%% business receiver
%% ===================================================================

handle_info({m, Attrs} = Msg, #state{socket = Socket} = State) ->
    send_msg_to_client(Msg),
    {noreply, State, State#state.heartbeat_timeout};
handle_info({gm, Attrs}, #state{socket = Socket} = State) ->
    send_msg_to_client(Msg),
    {noreply, State, State#state.heartbeat_timeout};
handle_info(Info, State) ->
    log:i("Unknown Info: ~p.~n", [Info]),
    {noreply, State, State#state.heartbeat_timeout}.


terminate(_Reason, #state{user = User}) ->
    session_manager:unregister(User).
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================

setopts(Swocket) ->
    inet:setopts(Swocket, [{active, 300}, {packet, 0}, binary]).

process_packet([H|T], #state{socket = Socket} = State) ->
    case H of
        {<<"r">>, Attrs} ->
            {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
            log:i("Got r id=~p~n", [MsgId]),

            RR = case lists:keyfind(<<"c">>, 1, Attrs) of
                {<<"c">>, <<"login">>} ->
                    {<<"user">>, UserInfo} = lists:keyfind(<<"user">>, 1, Attrs),
                    {ok, User} = parse_user_info(UserInfo),
                    session_manager:register(User, self()),
                    NewState = State#state{user = User},
                    <<"[rr] id=\"", MsgId/binary, "\" c=\"success\"">>;
                _ ->
                    NewState = State,
                    <<"[rr] id=\"", MsgId/binary, "\" c=\"error\"">>
            end,
            gen_tcp:send(Socket, RR);
        {<<"m">>, Attrs} = Msg ->
            send_ack(Socket, Attrs),

            case lists:keyfind(<<"to">>, 1, Attrs) of
                {<<"to">>, ToUserInfo} ->
                    {ok, ToUser} = parse_user_info(ToUserInfo),
                    send_msg_2_single_user(ToUser#user.id, Msg);
                _ ->
                    ignore
            end,
            NewState = State;
        {<<"gm">>, Attrs} = Msg ->
            send_ack(Socket, Attrs),

            case lists:keyfind(<<"group">>, 1, Attrs) of
                {<<"group">>, [{<<"id">>, GroupId}]} ->
                    % UserIdList = group:get_user_id_list(GroupId),
                    UserIdList = [<<"22">>, <<"23">>],
                    send_msg_2_multiple_users(UserIdList, Msg);
                _ ->
                    ignore
            end,
            NewState = State;
        {<<"a">>, Attrs} ->
            % hack:offline
            {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
            NewState = State
    end,
    process_toml(T, NewState);
process_packet([], NewState) ->
    {ok, NewState}.

% [{<<"device">>,<<"android">>},{<<"id">>,<<"1">>}]
parse_user_info(UserInfo) ->
    case UserInfo of
        [{<<"device">>, Device}, {<<"id">>, Id}] ->
            {ok, #user{id = Id, device = Device}};
        [{<<"id">>, Id}, {<<"device">>, Device}] ->
            {ok, #user{id = Id, device = Device}};
        _ ->
            {error, <<"user info parse failed">>}
    end.

send_ack(Socket, Attrs) ->
    {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
    Ack = <<"[a] id=\"", MsgId/binary, "\"">>,
    log:i("Got msg id=~p~n", [MsgId]),
    gen_tcp:send(Socket, Ack).

send_msg_2_single_user(UserId, Msg) ->
    case session_manager:get(UserId) of
        offline ->
            % hack: offline
            log:i("offline msg: ~p~n", [Msg]),
            ok;
        ToPidList ->
            send_msg_to_pid(ToPidList, Msg)
    end.

send_msg_2_multiple_users([H|T], Msg) ->
    send_msg_2_single_user(H, Msg),
    send_msg_2_multiple_users(T, Msg);
send_msg_2_multiple_users([], Msg) ->
    ok.


send_msg_to_pid([H|T], Msg) ->
    H ! {m, Msg},
    send_msg_to_pid(T, Msg);
send_msg_to_pid([], _) ->
    ok.


send_msg_to_client(Msg) ->
    Bin = tuple_to_toml(Msg),
    gen_tcp:send(Socket, Bin).

tuple_to_toml({Name, Attrs}) ->
    tuple_to_toml(Name, Attrs, <<"[", Name/binary, "]">>).
tuple_to_toml(Name, [H|T], Result) ->
    {Key, Value} = H,
    case is_