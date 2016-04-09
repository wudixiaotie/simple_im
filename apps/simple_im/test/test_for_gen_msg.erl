-module (test_for_gen_msg).

-behaviour (gen_msg).

-export ([start_link/0, test/1]).

-export ([init/1, handle_msg/2, terminate/2]).



start_link() ->
    gen_msg:start_link({local,?MODULE}, ?MODULE, []).

test(N) ->
    ?MODULE ! start,
    do_test(N).
do_test(N) when N > 0 ->
    ?MODULE ! a,
    do_test(N - 1);
do_test(0) ->
    ?MODULE ! stop,
    ok.


init([]) ->
    {ok, []}.

handle_msg(start, _State) ->
    timer:sleep(1000),
    {A,B,C} = os:timestamp(),
    Timestamp = (A * 1000000 + B) * 1000000 + C,
    {ok, [Timestamp, 0]};
handle_msg(stop, [Timestamp, Times]) ->
    {A,B,C} = os:timestamp(),
    Timestamp1 = (A * 1000000 + B) * 1000000 + C,
    Cost = Timestamp1 - Timestamp,
    io:format("======gen_msg:  Times:~p Cost:~p~n", [Times, Cost]),
    {ok, []};
handle_msg(_Msg, [Timestamp, Times]) ->
    {ok, [Timestamp, Times+1]}.

terminate(_Reason, _State) -> ok.