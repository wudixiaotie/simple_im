-module (tl).

-export ([t1/0, t2/0]).

t1() ->
    [ X || X <- lists:seq(1, 5000) ].

t2() ->
    t2(lists:seq(1, 5000)).
t2([H|T]) ->
    H,
    t2(T);
t2([]) ->
    ok.