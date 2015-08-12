-module (session_worker).

-behaviour (gen_server).

% APIs
-export([start_link/0, get/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

get(UserId) ->
    case ets:lookup(session, UserId) of
        [] ->
            offline;
        [{UserId, Pid}] ->
            Pid
    end.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, nomatch, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.

% {update, insert, {UserId, Pid}} | {update, delete, UserId}
handle_info({update, Type, Session}, State) ->
    ets:Type(session, Session),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================