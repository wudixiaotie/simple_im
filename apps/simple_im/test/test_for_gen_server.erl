
-module (test_for_gen_server).

-behaviour (gen_server).

% APIs
-export([start_link/0,test/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

test(N) ->
    ?MODULE ! start,
    do_test(N).
do_test(N) when N > 0 ->
    ?MODULE ! a,
    do_test(N - 1);
do_test(0) ->
    ?MODULE ! stop,
    ok.
    
    
%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, []}.
handle_call(_Request, _From, State) ->
    {reply, nomatch, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start, _State) ->
    timer:sleep(1000),
    {A,B,C} = os:timestamp(),
    Timestamp = (A * 1000000 + B) * 1000000 + C,
    {noreply, [Timestamp, 0]};
handle_info(stop, [Timestamp, Times]) ->
    {A,B,C} = os:timestamp(),
    Timestamp1 = (A * 1000000 + B) * 1000000 + C,
    Cost = Timestamp1 - Timestamp,
    io:format("======gen_server:  Times:~p Cost:~p~n", [Times, Cost]),
    {noreply, []};
handle_info(_Info, [Timestamp, Times]) ->
    {noreply, [Timestamp, Times+1]}.
terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================