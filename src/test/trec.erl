-module (trec).

-behaviour (gen_server).

% APIs
-export([start_link/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
    
%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    self() ! {recv_msg, 12},
    loop(20),
    {ok, []}.
handle_call(_Request, _From, State) ->
    {reply, nomatch, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info({recv_msg, N}, State) ->
    self() ! {test_msg, N, 1},
    MsgList = receive_all(N),
    io:format("recv_msg:~p~n", [MsgList]),
    {noreply, State};
handle_info(Info, State) ->
    io:format("handle_info:~p~n", [Info]),
    {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================
loop(N) when N > 0 ->
    self() ! {test_msg, N, N},
    loop(N - 1);
loop(0) ->
    ok.

receive_all(N) ->
    receive_all(N, []).
receive_all(N, MsgList) ->
    receive
        {test_msg, N, _} = Msg ->
            receive_all(N, [Msg | MsgList])
    after
        0 ->
            MsgList
    end.