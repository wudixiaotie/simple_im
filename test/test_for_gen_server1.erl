-module (test_for_gen_server1).

-behaviour (gen_server).

%% APIs
-export([test1/1, test2/1, test3/1, test4/1, test5/1, test6/1, test7/1,
         test8/1, test9/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

test1(N) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Pid ! {start, N}.

test2(N) -> Pid = spawn(fun() -> loop2([undefined, 0]) end), Pid ! {start, N}.
test3(N) -> Pid = spawn(fun() -> loop3([undefined, 0]) end), Pid ! {start, N}.
test4(N) -> Pid = spawn(fun() -> loop4([undefined, 0]) end), Pid ! {start, N}.
test5(N) -> Pid = spawn(fun() -> loop5([undefined, 0]) end), Pid ! {start, N}.
test6(N) -> Pid = spawn(fun() -> loop6([undefined, 0]) end), Pid ! {start, N}.
test7(N) -> Pid = spawn(fun() -> loop7([undefined, 0]) end), Pid ! {start, N}.
test8(N) -> Pid = spawn(fun() -> loop8(undefined, 0) end), Pid ! {start, N}.
test9(N) -> Pid = spawn(fun() -> loop9({undefined, 0}) end), Pid ! {start, N}.

%%==============================================================================

init([]) ->
    {ok, []}.
handle_call(_Request, _From, State) ->
    {reply, nomatch, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({start, N}, _State) ->
    do_test(N),
    {A,B,C} = os:timestamp(),
    Timestamp = (A * 1000000 + B) * 1000000 + C,
    {noreply, [Timestamp, 0]};
handle_info(stop, [Timestamp, Times]) ->
    {A,B,C} = os:timestamp(),
    Timestamp1 = (A * 1000000 + B) * 1000000 + C,
    Cost = Timestamp1 - Timestamp,
    io:format("======gen_server:  Times:~p Cost:~p~n", [Times, Cost]),
    {stop, normal, []};
handle_info(_Info, [Timestamp, Times]) ->
    {noreply, [Timestamp, Times + 1]}.

terminate(_Reason, _State) -> ok.

code_change(_OldVer, State, _Extra) -> {ok, State}.

do_test(0) -> self() ! stop;
do_test(N) -> self() ! a, do_test(N - 1).

%%==============================================================================

loop2(State) ->
    Msg = receive
              Input -> Input
          end,
    Reply = {ok, handle_info(Msg, State)},
    handle_common_reply(Reply, Msg, State).

handle_common_reply(Reply, _Msg, _State) ->
    case Reply of
        {ok, {noreply, NState}} -> loop2(NState);
        {ok, {stop, normal, _}} -> ok
    end.

%%==============================================================================

loop3(State) ->
    Msg = receive
              Input -> Input
          end,
    Reply = {ok, handle_info(Msg, State)},
    case Reply of
        {ok, {noreply, NState}} -> loop3(NState);
        {ok, {stop, normal, _}} -> ok
    end.

%%==============================================================================

loop4(State) ->
    Msg = receive
              Input -> Input
          end,
    case handle_info(Msg, State) of
        {noreply, NState} -> loop4(NState);
        {stop, normal, _} -> ok
    end.

%%==============================================================================

loop5(State) ->
    receive
        Input ->
            case handle_info(Input, State) of
                {noreply, NState} -> loop5(NState);
                {stop, normal, _} -> ok
            end
    end.

%%==============================================================================

loop6(State) ->
    receive
        {start, _N} = Msg ->
            {noreply, NState} = handle_info(Msg, State),
            loop6(NState);
        stop = Msg ->
            {stop, normal, []} = handle_info(Msg, State);
        Info ->
            {noreply, NState} = handle_info(Info, State),
            loop6(NState)
    end.

%%==============================================================================

loop7([Timestamp, Times]) ->
    receive
        {start, N} ->
            do_test(N),
            {A,B,C} = os:timestamp(),
            NTimestamp = (A * 1000000 + B) * 1000000 + C,
            loop7([NTimestamp, 0]);
        stop ->
            {A,B,C} = os:timestamp(),
            NTimestamp = (A * 1000000 + B) * 1000000 + C,
            Cost = NTimestamp - Timestamp,
            io:format("======Times:~p Cost:~p~n", [Times, Cost]);
        _Info ->
            loop7([Timestamp, Times + 1])
    end.

%%==============================================================================

loop8(Timestamp, Times) ->
    receive
        {start, N} ->
            do_test(N),
            {A,B,C} = os:timestamp(),
            NTimestamp = (A * 1000000 + B) * 1000000 + C,
            loop8(NTimestamp, 0);
        stop ->
            {A,B,C} = os:timestamp(),
            NTimestamp = (A * 1000000 + B) * 1000000 + C,
            Cost = NTimestamp - Timestamp,
            io:format("======Times:~p Cost:~p~n", [Times, Cost]);
        _Info ->
            loop8(Timestamp, Times + 1)
    end.

%%==============================================================================

loop9({Timestamp, Times}) ->
    receive
        {start, N} ->
            do_test(N),
            {A,B,C} = os:timestamp(),
            NTimestamp = (A * 1000000 + B) * 1000000 + C,
            loop9({NTimestamp, 0});
        stop ->
            {A,B,C} = os:timestamp(),
            NTimestamp = (A * 1000000 + B) * 1000000 + C,
            Cost = NTimestamp - Timestamp,
            io:format("======Times:~p Cost:~p~n", [Times, Cost]);
        _Info ->
            loop9({Timestamp, Times + 1})
    end.