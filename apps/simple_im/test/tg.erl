-module (tg).

-behaviour (gen_server).

% APIs
-export([start_link/1, start_link/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-compile (export_all).

start(N) ->
    ok = loop(N),
    tc:ct(gen_server, call, [1], 100000).

loop(N) when N > 0 ->
    start_link(N),
    loop(N - 1);
loop(0) -> ok.


start_link(tsup) ->
    {ok, erlang:whereis(tg)};
start_link(Name) ->
    gen_server:start_link({global, Name}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, []}.
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