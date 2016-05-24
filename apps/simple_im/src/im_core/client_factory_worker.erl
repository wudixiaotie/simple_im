%% ===================================================================
%% Author xiaotie
%% 2015-09-26
%% client factory worker
%% ===================================================================

-module(client_factory_worker).

-behaviour(gen_msg).

% APIs
-export([start_link/2]).

% gen_msg callbacks
-export([init/1, handle_msg/2, terminate/2]).

-record(state, {name :: atom(),
                client_factory :: atom(),
                ssl_configs :: list(),
                timer_ref}).

-include("device.hrl").
-include("message.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

start_link(Name, ClientFactory) ->
    gen_msg:start_link({local, Name}, ?MODULE, [Name, ClientFactory], []).


%% ===================================================================
%% gen_msg callbacks
%% ===================================================================

init([Name, ClientFactory]) ->
    {ok, SslConfigs} = utility:ssl_configs(),
    free_worker(Name, ClientFactory),
    {ok, #state{name = Name, client_factory = ClientFactory, ssl_configs = SslConfigs}}.


handle_msg({make, Socket}, State) ->
    NewState = case ssl:ssl_accept(Socket, State#state.ssl_configs) of
        {ok, SslSocket} -> 
            ok = ssl:setopts(SslSocket, [{active, once}, {packet, 0}, binary]),
            {ok, TimerRef} = timer:exit_after(10000, stuck),
            State#state{timer_ref = TimerRef};
        _ ->
            State
    end,
    {ok, NewState};
handle_msg({ssl, SslSocket, Bin}, State) ->
    timer:cancel(State#state.timer_ref),
    {ok, Toml} = toml:binary_2_term(Bin),
    [{<<"r">>, Attrs}|_] = Toml,
    {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
    {<<"user">>, UserInfo} = lists:keyfind(<<"user">>, 1, Attrs),
    case utility:check_parameters([<<"id">>, <<"d">>, <<"token">>], UserInfo) of
        {ok, [UserId, DeviceName, Token]} ->
            case lists:keyfind(<<"t">>, 1, Attrs) of
                {<<"t">>, <<"login">>} ->
                    UserIdBin = erlang:integer_to_binary(UserId),
                    {ok, TokenKey} = redis:key({token, Token}),
                    case redis:q([<<"HMGET">>, TokenKey, <<"user_id">>, <<"device">>]) of
                        {ok, undefined} ->
                            send_error(SslSocket, MsgId, <<"Token Error">>);
                        {ok, [UserIdBin, DeviceName]} ->
                            {ok, <<"1">>} = redis:q([<<"PERSIST">>, TokenKey]),
                            case session:find(UserId) of
                                offline ->
                                    RR = {<<"rr">>,
                                          [{<<"id">>, MsgId},
                                           {<<"s">>, 0}]},
                                    {ok, RRBin} = toml:term_2_binary(RR),
                                    Message = #message{id = MsgId, bin = RRBin},
                                    Device = #device{name = DeviceName,
                                                     ssl_socket = SslSocket,
                                                     token = Token},
                                    {ok, Pid} = supervisor:start_child(client_sup, [Message, UserId, Device]),
                                    log:i("[IM] Start a new client ~p ~p~n", [{UserId, Device}, Pid]),
                                    ok = ssl:controlling_process(Device#device.ssl_socket, Pid);
                                {ok, Pid} ->
                                    RR = {<<"rr">>,
                                          [{<<"id">>, MsgId},
                                           {<<"s">>, 0}]},
                                    {ok, RRBin} = toml:term_2_binary(RR),
                                    Message = #message{id = MsgId, bin = RRBin},
                                    Device = #device{name = DeviceName,
                                                     ssl_socket = SslSocket,
                                                     token = Token},
                                    Node = erlang:node(),
                                    case catch erlang:node(Pid) of
                                        Node ->
                                            Pid ! {replace_socket, Message, Device},
                                            ok = ssl:controlling_process(Device#device.ssl_socket, Pid);
                                        _ ->
                                            {ok, NewPid} = supervisor:start_child(client_sup, [Message, UserId, Device]),
                                            log:i("[IM] Start a new client of userid ~p to replace old client ~p~n", [UserId, NewPid]),
                                            ok = ssl:controlling_process(Device#device.ssl_socket, NewPid)
                                    end;
                                {error, _} ->
                                    send_error(SslSocket, MsgId, <<"Unknown error">>)
                            end;
                        _ ->
                            log:e("[IM] Start client error: ~p~n", [<<"Token not match">>]),
                            RR = {<<"rr">>,
                                  [{<<"id">>, MsgId},
                                   {<<"t">>, <<"login">>},
                                   {<<"s">>, 1},
                                   {<<"r">>, <<"Token not match">>}]},
                            {ok, RRBin} = toml:term_2_binary(RR),
                            ok = ssl:send(SslSocket, RRBin),
                            ok = ssl:close(SslSocket)
                    end;
                _ ->
                    send_error(SslSocket, MsgId, <<"Unknown request">>)
            end;
        {error, Reason} ->
            send_error(SslSocket, MsgId, Reason)
    end,
    free_worker(State#state.name, State#state.client_factory),
    timer:cancel(State#state.timer_ref),
    {ok, State#state{timer_ref = undefined}};
handle_msg(_Info, State) -> {ok, State}.


terminate(_Reason, _State) -> ok.


%% ===================================================================
%% Internal functions
%% ===================================================================

free_worker(Name, ClientFactory) ->
    ClientFactory ! {free_worker, Name}.


send_error(SslSocket, MsgId, Reason) ->
    log:e("[IM] Start client error: ~p~n", [Reason]),
    RR = {<<"rr">>,
          [{<<"id">>, MsgId},
           {<<"s">>, 1},
           {<<"r">>, Reason}]},
    {ok, RRBin} = toml:term_2_binary(RR),
    ok = ssl:send(SslSocket, RRBin),
    ok = ssl:close(SslSocket).