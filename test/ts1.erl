-module (ts1).

-export ([t1/0, t2/0, t3/0, t4/0]).

t1() ->
    "cf_worker_" ++ "123".

t2() ->
    A = "321" ++ "_rekrow_fc",
    lists:reverse(A).

t3() ->
    lists:merge("cf_worker_", "123").

t4() ->
    ok.