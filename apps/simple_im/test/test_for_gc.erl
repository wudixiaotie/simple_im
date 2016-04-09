-module (test_for_gc).

-export ([test1/1, test2/1, test3/1, test4/1, test5/1, test6/1]).
-export ([bigheap2/2, bigheap3/2, bigheap4/2, bigheap5/2, bigheap6/2]).

-record (state, {timestamp, times}).

send_msg(0) ->
    self() ! stop;
send_msg(N) ->
    self() ! a,
    send_msg(N-1).

% test for running function without GC
test1(N) ->
    Pid = spawn(fun() -> loop1(undefined, 0) end), Pid ! {start, N}.

loop1(Timestamp, Times) ->
    receive
        {start, N} ->
            send_msg(N),
            {A,B,C} = os:timestamp(),
            NTimestamp = (A * 1000000 + B) * 1000000 + C,
            loop1(NTimestamp, 0);
        stop ->
            {A,B,C} = os:timestamp(),
            NTimestamp = (A * 1000000 + B) * 1000000 + C,
            Cost = NTimestamp - Timestamp,
            io:format("======Times:~p Cost:~p~n", [Times, Cost]);
        _Info ->
            loop1(Timestamp, Times + 1)
    end.


% test for running function with record GC
test2(N) ->
    Pid = spawn(fun() -> loop2(#state{timestamp = undefined, times = 0}) end), Pid ! {start, N}.
bigheap2(N, Size) ->
    Pid = spawn_opt(fun() -> loop2(#state{timestamp = undefined, times = 0}) end, [{min_heap_size, Size}]), Pid ! {start, N}.

loop2(State) ->
    receive
        {start, N} ->
            send_msg(N),
            {A,B,C} = os:timestamp(),
            NTimestamp = (A * 1000000 + B) * 1000000 + C,
            NewState = #state{timestamp = NTimestamp, times = 0},
            loop2(NewState);
        stop ->
            {A,B,C} = os:timestamp(),
            NTimestamp = (A * 1000000 + B) * 1000000 + C,
            Cost = NTimestamp - State#state.timestamp,
            io:format("======Times:~p Cost:~p~n", [State#state.times, Cost]);
        _Info ->
            NewState = State#state{times = State#state.times + 1},
            loop2(NewState)
    end.


% test for running function with map GC
test3(N) ->
    Pid = spawn(fun() -> loop3(#{timestamp => undefined, times => 0}) end), Pid ! {start, N}.
bigheap3(N, Size) ->
    Pid = spawn_opt(fun() -> loop3(#{timestamp => undefined, times => 0}) end, [{min_heap_size, Size}]), Pid ! {start, N}.

loop3(State) ->
    receive
        {start, N} ->
            send_msg(N),
            {A,B,C} = os:timestamp(),
            NTimestamp = (A * 1000000 + B) * 1000000 + C,
            NewState = State#{timestamp => NTimestamp},
            loop3(NewState);
        stop ->
            {A,B,C} = os:timestamp(),
            NTimestamp = (A * 1000000 + B) * 1000000 + C,
            #{timestamp := Timestamp, times := Times} = State,
            Cost = NTimestamp - Timestamp,
            io:format("======Times:~p Cost:~p~n", [Times, Cost]);
        _Info ->
            #{times := Times} = State,
            NewState = State#{times => Times + 1},
            loop3(NewState)
    end.


% test for running function with map GC
test4(N) ->
    Pid = spawn(fun() -> loop4(#{timestamp => undefined, times => 0}) end), Pid ! {start, N}.
bigheap4(N, Size) ->
    Pid = spawn_opt(fun() -> loop4(#{timestamp => undefined, times => 0}) end, [{min_heap_size, Size}]), Pid ! {start, N}.

loop4(State) ->
    receive
        {start, N} ->
            send_msg(N),
            {A,B,C} = os:timestamp(),
            NTimestamp = (A * 1000000 + B) * 1000000 + C,
            NewState = State#{timestamp => NTimestamp},
            loop4(NewState);
        stop ->
            {A,B,C} = os:timestamp(),
            NTimestamp = (A * 1000000 + B) * 1000000 + C,
            #{timestamp := Timestamp, times := Times} = State,
            Cost = NTimestamp - Timestamp,
            io:format("======Times:~p Cost:~p~n", [Times, Cost]);
        _Info ->
            {ok, Times} = maps:find(times, State),
            NewState = State#{times => Times + 1},
            loop4(NewState)
    end.


% test for running function with list GC
test5(N) ->
    Pid = spawn(fun() -> loop5([undefined, 0]) end), Pid ! {start, N}.
bigheap5(N, Size) ->
    Pid = spawn_opt(fun() -> loop5([undefined, 0]) end, [{min_heap_size, Size}]), Pid ! {start, N}.

loop5([Timestamp, Times]) ->
    receive
        {start, N} ->
            send_msg(N),
            {A,B,C} = os:timestamp(),
            NTimestamp = (A * 1000000 + B) * 1000000 + C,
            loop5([NTimestamp, Times]);
        stop ->
            {A,B,C} = os:timestamp(),
            NTimestamp = (A * 1000000 + B) * 1000000 + C,
            Cost = NTimestamp - Timestamp,
            io:format("======Times:~p Cost:~p~n", [Times, Cost]);
        _Info ->
            loop5([Timestamp, Times + 1])
    end.


% test for running function with tuple GC
test6(N) ->
    Pid = spawn(fun() -> loop6({undefined, 0}) end), Pid ! {start, N}.
bigheap6(N, Size) ->
    Pid = spawn_opt(fun() -> loop6({undefined, 0}) end, [{min_heap_size, Size}]), Pid ! {start, N}.

loop6({Timestamp, Times}) ->
    receive
        {start, N} ->
            send_msg(N),
            {A,B,C} = os:timestamp(),
            NTimestamp = (A * 1000000 + B) * 1000000 + C,
            loop6({NTimestamp, Times});
        stop ->
            {A,B,C} = os:timestamp(),
            NTimestamp = (A * 1000000 + B) * 1000000 + C,
            Cost = NTimestamp - Timestamp,
            io:format("======Times:~p Cost:~p~n", [Times, Cost]);
        _Info ->
            loop6({Timestamp, Times + 1})
    end.