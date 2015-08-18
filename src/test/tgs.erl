-module (tgs).

-behaviour (gen_server).

% APIs
-export([start_link/0, worker/0, worker1/0, loop/1, t/0, t1/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    erlang:register(worker, spawn(fun ?MODULE:worker/0)),
    erlang:register(worker1, spawn(fun ?MODULE:worker1/0)),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

worker() ->
    receive
        _ ->
            worker()
    end.

loop(Dest) ->
    io:format("loop start:~p~n", [os:timestamp()]),
    loop(Dest, 100000).
loop(Dest, N) when N > 0 ->
    Dest ! a,
    loop(Dest, N - 1);
loop(_, 0) ->
    io:format("loop finish:~p~n", [os:timestamp()]),
    ok.



t() ->
    io:format("handle_info t start:~p~n", [os:timestamp()]),
    t(50000).
t(N) when N > 0 ->
    ?MODULE ! t,
    t(N - 1);
t(0) ->
    ?MODULE ! tf,
    io:format("handle_info t finish send:~p~n", [os:timestamp()]),
    ok.

t1() ->
    io:format(" t1 start:~p~n", [os:timestamp()]),
    t1(100000).
t1(N) when N > 0 ->
    worker1 ! a,
    t1(N - 1);
t1(0) ->
    worker1 ! tf,
    io:format(" t1 finish send:~p~n", [os:timestamp()]),
    ok.

worker1() ->
    receive
        a ->
            worker ! t;
        _ ->
            io:format(" t1 finish:~p~n", [os:timestamp()]),
            ok
    end,
    worker1().

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, []}.
handle_call(_Request, _From, State) ->
    {reply, nomatch, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(t, State) ->
    worker ! a,
    % offline_worker_name(1),
    {noreply, State};
handle_info(tf, State) ->
    io:format("handle_info t finish:~p ~n", [os:timestamp()]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================

offline_worker_name(Index) ->
    IndexStr = erlang:integer_to_list(Index),
    erlang:list_to_atom("offline_worker_" ++ IndexStr).