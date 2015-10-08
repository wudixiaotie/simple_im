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

-include("user.hrl").



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
    {ok, TimerRef} = timer:send_after(2000, free),
    {noreply, State#state{timer_ref = TimerRef}};
handle_info(free, #state{timer_ref = undefined} = State) ->
    {noreply, State};
handle_info(free, State) ->
    free_worker(State#state.name),
    timer:cancel(State#state.timer_ref),
    {noreply, State#state{timer_ref = undefined}};
handle_info({tcp, Socket, Data}, State) ->
    timer:cancel(State#state.timer_ref),
    {ok, Toml} = toml:binary_2_term(Data),
    [{<<"r">>, Attrs}|_] = Toml,
    {<<"id">>, MsgId} = lists:keyfind(<<"id">>, 1, Attrs),
    {<<"user">>, UserInfo} = lists:keyfind(<<"user">>, 1, Attrs),
    {ok, User} = users:parse(UserInfo),
    case lists:keyfind(<<"t">>, 1, Attrs) of
        {<<"t">>, <<"login">>} ->
            UserIdBin = erlang:integer_to_binary(User#user.id),
            TokenKey = redis:key({token, User#user.token}),
            case redis:q([<<"HGET">>, TokenKey, <<"user_id">>]) of
                {ok, undefined} ->
                    RR = {<<"rr">>,
                          [{<<"id">>, MsgId},
                           {<<"t">>, <<"login">>},
                           {<<"s">>, 1},
                           {<<"r">>, <<"Token error">>}]},
                    {ok, RRBin} = toml:term_2_binary(RR),
                    gen_tcp:send(Socket, RRBin),
                    gen_tcp:close(Socket);
                {ok, UserIdBin} ->
                    {ok, <<"1">>} = redis:q([<<"PERSIST">>, TokenKey]),
                    case session:verify(User) of
                        offline ->
                            RR = {<<"rr">>,
                                  [{<<"id">>, MsgId},
                                   {<<"t">>, <<"login">>},
                                   {<<"s">>, 0}]},
                            new_client(Socket, RR, User);
                        {ok, Pid} ->
                            RR = {<<"rr">>,
                                  [{<<"id">>, MsgId},
                                   {<<"t">>, <<"login">>},
                                   {<<"s">>, 0}]},
                            client_change_socket(Pid, Socket, RR, User);
                        {error, <<"Wrong device">>} ->
                            RR = {<<"rr">>,
                                  [{<<"id">>, MsgId},
                                   {<<"t">>, <<"login">>},
                                   {<<"s">>, 1},
                                   {<<"r">>, <<"Wrong device">>}]},
                            {ok, RRBin} = toml:term_2_binary(RR),
                            gen_tcp:send(Socket, RRBin),
                            gen_tcp:close(Socket);
                        {error, _} ->
                            RR = {<<"rr">>,
                                  [{<<"id">>, MsgId},
                                   {<<"t">>, <<"login">>},
                                   {<<"s">>, 0}]},
                            new_client(Socket, RR, User)
                    end;
                _ ->
                    RR = {<<"rr">>,
                          [{<<"id">>, MsgId},
                           {<<"t">>, <<"login">>},
                           {<<"s">>, 1},
                           {<<"r">>, <<"Unknown error">>}]},
                    {ok, RRBin} = toml:term_2_binary(RR),
                    gen_tcp:send(Socket, RRBin),
                    gen_tcp:close(Socket)
            end;
        {<<"t">>, <<"reconnect">>} ->
            case session:verify(User) of
                offline ->
                    RR = {<<"rr">>,
                          [{<<"id">>, MsgId},
                           {<<"t">>, <<"reconnect">>},
                           {<<"s">>, 1},
                           {<<"r">>, <<"offline">>}]},
                    {ok, RRBin} = toml:term_2_binary(RR),
                    gen_tcp:send(Socket, RRBin),
                    gen_tcp:close(Socket);
                {ok, Pid} ->
                    RR = {<<"rr">>,
                          [{<<"id">>, MsgId},
                           {<<"t">>, <<"reconnect">>},
                           {<<"s">>, 0}]},
                    client_change_socket(Pid, Socket, RR, User);
                {error, Reason} ->
                    RR = {<<"rr">>,
                          [{<<"id">>, MsgId},
                           {<<"t">>, <<"reconnect">>},
                           {<<"s">>, 1},
                           {<<"r">>, Reason}]},
                    {ok, RRBin} = toml:term_2_binary(RR),
                    gen_tcp:send(Socket, RRBin),
                    gen_tcp:close(Socket)
            end;
        _ ->
            RR = {<<"rr">>,
                  [{<<"id">>, MsgId},
                   {<<"s">>, 1},
                   {<<"r">>, <<"Unknown request">>}]},
            {ok, RRBin} = toml:term_2_binary(RR),
            gen_tcp:send(Socket, RRBin),
            gen_tcp:close(Socket)
    end,
    free_worker(State#state.name),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================

free_worker(Name) ->
    cf ! {free_worker, Name}.


new_client(Socket, RR, User) ->
    {ok, Pid} = supervisor:start_child(client_sup, [Socket, RR, User]),
    log:i("Start a new client ~p ~p~n", [User, Pid]),
    gen_tcp:controlling_process(Socket, Pid).


client_change_socket(Pid, Socket, RR, User) ->
    Node = node(),
    case node(Pid) of
        Node ->
            Pid ! {new_socket, Socket, RR},
            gen_tcp:controlling_process(Socket, Pid);
        _ ->
            new_client(Socket, RR, User)
    end.