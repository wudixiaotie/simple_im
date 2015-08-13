-module (tgen_server).

-behaviour (gen_server).

% APIs
-export([start_link/0, t/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).


%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

t() ->
    ?MODULE ! t,
    t().

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    ets:new(session, [named_table]),
    erlang:send_after(5000, self(), stop),
    {ok, 0}.

handle_call(_Request, _From, State) ->
    {reply, nomatch, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% {update, insert, {UserId, Pid}} | {update, delete, UserId}
handle_info(t, State) ->
    ets:lookup(session, a),
    {noreply, State + 1};
handle_info(stop, State) ->
    io:format("~p~n", [State]),
    exit(self(), a),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================