-module (session).

-behaviour (gen_server).

% APIs
-export([start_link/0, get/1, register/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(UserId) ->
    ets:lookup(session, UserId).

register(UserId, Pid) ->
    ets:insert(session, {UserId, Pid}).

    
    
%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    ets:new(session, [named_table, {read_concurrency, true}, {write_concurrency, true}]),
    FatherNode = env:get(father_node),
    case FatherNode == node() of
        true ->
    loop(10000000),
            ok;
        false ->
            log:i("~p start copy session data from ~p~n", [node(), FatherNode]),
            SessionList = gen_server:call({?MODULE, FatherNode}, copy, infinity),
            init_session(SessionList)
    end,
    {ok, []}.

handle_call(copy, _From, State) ->
    SessionList = ets:tab2list(session),
    {reply, SessionList, State};
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
    ets:insert(session, {<<"user_", (integer_to_binary(N))/binary, "@android">>, N}),
    loop(N - 1);
loop(0) ->
    ok.

init_session([H|T]) ->
    ets:insert(session, H),
    init_session(T);
init_session([]) ->
    ok.