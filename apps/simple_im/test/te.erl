%% ===================================================================
%% Author xiaotie
%% 2015-08-02
%% test ets table
%% ===================================================================

-module (te).

-behaviour (gen_server).

% APIs
-export([start_link/0, t/0, ct/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
ct() ->
    tc:ct(ets, lookup, [test_ets, <<"user_1@android">>], 100000).
t() ->
    tc:t(ets, lookup, [test_ets, <<"user_1@android">>], 1000).
    
    
%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    ets:new(test_ets, [named_table, {write_concurrency, true}]),
    loop(10000000),
    {ok, []}.
handle_call({insert, UserId}, _From, State) ->
    {reply, ets:lookup(test_ets, UserId), State};
handle_call(_Request, _From, State) ->
    {reply, nomatch, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================
loop(N) when N > 0 ->
    ets:insert(test_ets, {<<"user_", (integer_to_binary(N))/binary, "@android">>, N}),
    loop(N - 1);
loop(0) ->
    ok.