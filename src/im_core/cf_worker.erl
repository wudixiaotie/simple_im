%% ===================================================================
%% Author xiaotie
%% 2015-9-26
%% client factory worker
%% ===================================================================

-module (cf_worker).

-behaviour(gen_server).

% APIs
-export([start_link/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {name :: atom()}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Name]) ->
    free_worker(Name),
    {ok, #state{name = Name}}.
handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================

free_worker(Name) ->
    cf ! {free, Name}.