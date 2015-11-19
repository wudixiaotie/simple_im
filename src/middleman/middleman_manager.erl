%% ===================================================================
%% Author xiaotie
%% 2015-11-19
%% manager accept reuqest, start worker
%% ===================================================================

-module(middleman_manager).

-behaviour(gen_server).

% APIs
-export([start_link/0, register/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).



%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


register() ->
    gen_server:call(?MODULE, register).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    ets:new(roster, [named_table, public, {read_concurrency, true}]),
    true = ets:insert(roster, {max, 0}),
    {ok, []}.


handle_call(register, _From, State) ->
    Max = ets:lookup_element(roster, max, 2),
    NewMax = Max + 1,
    true = ets:insert(roster, {max, NewMax}),
    {reply, {ok, NewMax}, State};
handle_call(_Request, _From, State) -> {reply, nomatch, State}.


handle_cast(_Msg, State) -> {noreply, State}.


handle_info(unregister, State) ->
    Max = ets:lookup_element(roster, max, 2),
    true = ets:insert(roster, {max, Max - 1}),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================