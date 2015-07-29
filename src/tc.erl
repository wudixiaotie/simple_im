-module (tc).

-export ([t/4, ct/4]).


tc(M, F, A) ->
    {Microsecond, _} = timer:tc (M, F, A),
    Microsecond.

%% ===================================================================
%% test: one process test N times
%% ===================================================================

t(M, F, A, N) ->
    {Max, Min, Sum, Aver} = loop ({M, F, A}, N),
    io:format ("=====================~n"),
    io:format ("execute [~p] times of {~p, ~p ~p}:~n", [N, M, F, A]),
    io:format ("Maximum: ~p(μs)\t~p(s)~n", [Max, Max / 1000000]),
    io:format ("Minimum: ~p(μs)\t~p(s)~n", [Min, Min / 1000000]),
    io:format ("Sum: ~p(μs)\t~p(s)~n", [Sum, Sum / 1000000]),
    io:format ("Average: ~p(μs)\t~p(s)~n", [Aver, Aver / 1000000]),
    io:format ("=====================~n").


loop({M, F, A}, N) ->
    loop ({M, F, A}, N, 1, 0, 0, 0).

loop({M, F, A}, N, I, Max, Min, Sum) when N >= I ->
    Microsecond = tc (M, F, A),
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
    loop ({M, F, A}, N, I + 1, NewMax, NewMin, NewSum);
loop({_M, _F, _A}, N, _I, Max, Min, Sum) ->
    {Max, Min, Sum, Sum / N}.

%% ===================================================================
%% Concurrency test: N processes each test one time
%% ===================================================================

ct(M, F, A, N) ->
    {Max, Min, Sum, Aver} = cloop ({M, F, A}, N),
    io:format ("=====================~n"),
    io:format ("spawn [~p] processes of {~p, ~p ~p}:~n", [N, M, F, A]),
    io:format ("Maximum: ~p(μs)\t~p(s)~n", [Max, Max / 1000000]),
    io:format ("Minimum: ~p(μs)\t~p(s)~n", [Min, Min / 1000000]),
    io:format ("Sum: ~p(μs)\t~p(s)~n", [Sum, Sum / 1000000]),
    io:format ("Average: ~p(μs)\t~p(s)~n", [Aver, Aver / 1000000]),
    io:format ("=====================~n").


cloop({M, F, A}, N) ->
    CollectorPid = self(),
    ok = loop_spawn({M, F, A}, CollectorPid, N),
    collector(0, 0, 0, N, 1).


loop_spawn({M, F, A}, CollectorPid, N) when N > 0 ->
    spawn(fun() -> worker({M, F, A}, CollectorPid) end),
    loop_spawn({M, F, A}, CollectorPid, N - 1);
loop_spawn(_, _, 0) ->
    ok.


collector(Max, Min, Sum, N, I) when N >= I ->
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
            collector(NewMax, NewMin, NewSum, N, I + 1)
    after
        5000 ->
            ok
    end;
collector(Max, Min, Sum, N, _) ->
    {Max, Min, Sum, Sum / N}.


worker({M, F, A}, CollectorPid) ->
    Microsecond = tc(M, F, A),
    CollectorPid ! {result, Microsecond}.