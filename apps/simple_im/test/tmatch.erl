-module (tmatch).

-behaviour (gen_server).

% APIs
-export([start_link/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (message, {id, toml}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    io:format("=============~p~n", [self()]),
    {ok, []}.
handle_call(_Request, _From, State) ->
    {reply, nomatch, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(#message{} = Message, State) ->
    io:format("1=============~p~n", [Message]),
    {noreply, State};
handle_info(_Info, State) ->
    io:format("=============~p~n", [_Info]),
    {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================