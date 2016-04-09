-module (ts).

-export ([start_rec/0, send/0, start/1]).

start_rec() ->
    Pid = spawn(fun() -> rec() end),
    case whereis(rec) of
        undefined ->
            register(rec, Pid);
        _ ->
            ok
    end.


rec() ->
    receive
        stop ->
            stop;
        _ ->
            rec()
    end.

send() ->
    send(1000).

send(N) when N > 1 ->
    {rec, 's2@192.168.1.137'} ! a,
    send(N - 1);
send(1) ->
    {Microsecond, _} = timer:tc(erlang, send, [{rec, 's2@192.168.1.137'},a]),
    collector ! {result, Microsecond},
    ok.

collector(Max, Min, Sum, N, I, List) when N >= I ->
    receive
        {result, Microsecond} ->
            NewSum = Sum + Microsecond,
            if
                Max == 0 ->
                    NewMax = NewMin = Microsecond;
                Max < Microsecond ->
                    NewMax = Microsecond,
                    NewMin = Min;
                Min > Microsecond ->
                    NewMax = Max,
                    NewMin = Microsecond;
                true ->
                    NewMax = Max,
                    NewMin = Min
            end,
            collector(NewMax, NewMin, NewSum, N, I + 1, [Microsecond|List])
    end;
collector(Max, Min, Sum, N, _, List) ->
    Aver = Sum / N,
    {Less5, Less10, Less20, Less50, Less100, Less500, Other} = distribution(List, Aver),
    {Max, Min, Sum, Aver, Less5, Less10, Less20, Less50, Less100, Less500, Other}.

distribution(List, Aver) ->
    distribution(List, Aver, 0, 0, 0, 0, 0, 0, 0).
distribution([H|T], Aver, Less5, Less10, Less20, Less50, Less100, Less500, Other) ->
    if
        H < 5 ->
            distribution(T, Aver, Less5 + 1, Less10, Less20, Less50, Less100, Less500, Other);
        H < 10 ->
            distribution(T, Aver, Less5, Less10 + 1, Less20, Less50, Less100, Less500, Other);
        H < 20 ->
            distribution(T, Aver, Less5, Less10, Less20 + 1, Less50, Less100, Less500, Other);
        H < 50 ->
            distribution(T, Aver, Less5, Less10, Less20, Less50 + 1, Less100, Less500, Other);
        H < 100 ->
            distribution(T, Aver, Less5, Less10, Less20, Less50, Less100 + 1, Less500, Other);
        H < 500 ->
            distribution(T, Aver, Less5, Less10, Less20, Less50, Less100, Less500 + 1, Other);
        true ->
            distribution(T, Aver, Less5, Less10, Less20, Less50, Less100, Less500, Other + 1)
    end;
distribution([], _Aver, Less5, Less10, Less20, Less50, Less100, Less500, Other) ->
    {Less5, Less10, Less20, Less50, Less100, Less500, Other}.

start(N) ->
    case whereis(collector) of
        undefined ->
            register(collector, self());
        _ ->
            ok
    end,
    loop(N),
    {Max, Min, Sum, Aver, Less5, Less10, Less20, Less50, Less100, Less500, Other} = collector(0, 0, 0, N, 1, []),
    io:format ("=====================~n"),
    io:format ("spawn [~p] processes of erlang:send/3:~n", [N]),
    io:format ("Maximum: ~p(μs)\t~p(s)~n", [Max, Max / 1000000]),
    io:format ("Minimum: ~p(μs)\t~p(s)~n", [Min, Min / 1000000]),
    io:format ("Sum: ~p(μs)\t~p(s)~n", [Sum, Sum / 1000000]),
    io:format ("Average: ~p(μs)\t~p(s)~n", [Aver, Aver / 1000000]),
    io:format ("0   <= x < 5:   ~p~n", [Less5]),
    io:format ("5   <= x < 10:  ~p~n", [Less10]),
    io:format ("10  <= x < 20:  ~p~n", [Less20]),
    io:format ("20  <= x < 50:  ~p~n", [Less50]),
    io:format ("50  <= x < 100: ~p~n", [Less100]),
    io:format ("100 <= x < 500: ~p~n", [Less500]),
    io:format ("x => 500:       ~p~n", [Other]),
    io:format ("=====================~n").

loop(N) when N > 0 ->
    spawn(fun ts:send/0),
    loop(N - 1);
loop(0) ->
    ok.