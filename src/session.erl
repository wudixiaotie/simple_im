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
    Node = env:get(session_node),
    case Node == node() of
        true ->
            ok;
        false ->
            gen_server:call({?MODULE, Node}, copy)
    end,
    {ok, []}.
handle_call(_Request, _From, State) ->
    {reply, node(), State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================