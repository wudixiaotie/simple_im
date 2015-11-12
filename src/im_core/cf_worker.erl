%% ===================================================================
%% Author xiaotie
%% 2015-9-26
%% client factory worker
%% ===================================================================

-module (cf_worker).

-behaviour(gen_server).

% APIs
-export([start_link/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {name :: atom(), timer_ref}).

-include("device.hrl").
-include("message.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Name]) ->
    free_worker(Name),
    {ok, #state{name = Name}}.


handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.


handle_info({make, Socket}, State) ->
    inet:setopts(Socket, [{active, once}, {packet, 0}, binary]),
    gen_tcp:send(Socket, <<"ready">>),
    {ok, TimerRef} = timer:exit_after(10000, stuck),
    {noreply, State#state{timer_ref = TimerRef}};
handle_info({tcp, Socket, Data}, State) ->
    timer:cancel(State#state.timer_ref),
    {ok, Toml} = toml:binary_2_term(Data),
    [{<<"r">>, Attrs}|_] = Toml,
    {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
    {<<"user">>, UserInfo} = lists:keyfind(<<"user">>, 1, Attrs),
    case utility:check_parameters([<<"id">>, <<"device">>, <<"token">>], UserInfo) of
        {ok, [UserId, DeviceName, Token]} ->
            case lists:keyfind(<<"t">>, 1, Attrs) of
                {<<"t">>, <<"login">>} ->
                    UserIdBin = erlang:integer_to_binary(UserId),
                    {ok, TokenKey} = redis:key({token, Token}),
                    case redis:q([<<"HGET">>, TokenKey, <<"user_id">>]) of
                        {ok, undefined} ->
                            send_error(Socket, MsgId, <<"Token Error">>);
                        {ok, UserIdBin} ->
                            {ok, <<"1">>} = redis:q([<<"PERSIST">>, TokenKey]),
                            case session:find(UserId) of
                                offline ->
                                    RR = {<<"rr">>,
                                          [{<<"id">>, MsgId},
                                           {<<"s">>, 0}]},
                                    {ok, RRBin} = toml:term_2_binary(RR),
                                    Message = #message{id = MsgId, bin = RRBin},
                                    Device = #device{name = DeviceName,
                                                     socket = Socket,
                                                     token = Token},
                                    {ok, Pid} = supervisor:start_child(client_sup, [Message, UserId, Device]),
                                    log:i("Start a new client ~p ~p~n", [{UserId, Device}, Pid]),
                                    gen_tcp:controlling_process(Device#device.socket, Pid);
                                {ok, Pid} ->
                                    RR = {<<"rr">>,
                                          [{<<"id">>, MsgId},
                                           {<<"s">>, 0}]},
                                    {ok, RRBin} = toml:term_2_binary(RR),
                                    Message = #message{id = MsgId, bin = RRBin},
                                    Device = #device{name = DeviceName,
                                                     socket = Socket,
                                                     token = Token},
                                    Node = node(),
                                    case node(Pid) of
                                        Node ->
                                            Pid ! {replace_socket, Message, Device},
                                            gen_tcp:controlling_process(Device#device.socket, Pid);
                                        _ ->
                                            {ok, Pid} = supervisor:start_child(client_sup, [Message, UserId, Device]),
                                            log:i("Start a new client ~p ~p~n", [{UserId, Device}, Pid]),
                                            gen_tcp:controlling_process(Device#device.socket, Pid)
                                    end;
                                {error, _} ->
                                    send_error(Socket, MsgId, <<"Unknown error">>)
                            end;
                        _ ->
                            RR = {<<"rr">>,
                                  [{<<"id">>, MsgId},
                                   {<<"t">>, <<"login">>},
                                   {<<"s">>, 1},
                                   {<<"r">>, <<"Token not match">>}]},
                            {ok, RRBin} = toml:term_2_binary(RR),
                            gen_tcp:send(Socket, RRBin),
                            gen_tcp:close(Socket)
                    end;
                _ ->
                    send_error(Socket, MsgId, <<"Unknown request">>)
            end;
        {error, Reason} ->
            send_error(Socket, MsgId, Reason)
    end,
    free_worker(State#state.name),
    timer:cancel(State#state.timer_ref),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================

free_worker(Name) ->
    cf ! {free_worker, Name}.


send_error(Socket, MsgId, Reason) ->
    RR = {<<"rr">>,
          [{<<"id">>, MsgId},
           {<<"s">>, 1},
           {<<"r">>, Reason}]},
    {ok, RRBin} = toml:term_2_binary(RR),
    gen_tcp:send(Socket, RRBin),
    gen_tcp:close(Socket).