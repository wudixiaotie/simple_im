-module(gen_msg).

% APIs
-export([start_link/2, init/3, start_link/3, init/4]).

% system message
-export([system_continue/3, system_terminate/4, system_get_state/1,
         system_replace_state/2]).



%% ===================================================================
%% APIs
%% ===================================================================

-callback init(Args :: term()) -> {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate}.
-callback handle_msg(Msg :: term(), State :: term()) -> {ok, NewState :: term()} | {ok, State :: term(), timeout() | hibernate}.
-callback terminate(Reason :: term(), State :: term()) -> ok.

start_link(Module, Args) ->
    proc_lib:start_link(?MODULE, init, [self(), Module, Args]).


start_link(Name, Module, Args) ->
    proc_lib:start_link(?MODULE, init, [self(), Name, Module, Args]).


init(Parent, Module, Args) ->
    do_init(Parent, Module, Args).


init(Parent, Name, Module, Args) ->
    ok = register_name(Name),
    do_init(Parent, Module, Args).



%% ===================================================================
%% system message
%% ===================================================================

system_continue(Parent, Debug, [State, Module, Timeout]) ->
    loop(Parent, Debug, State, Module, Timeout).


system_terminate(Reason, _Parent, _Debug, [State, Module, _Timeout]) ->
    terminate(Reason, Module, State).


system_get_state([State, _Module, _Timeout]) ->
    {ok, State, State}.


system_replace_state(StateFun, State) ->
    NewState = StateFun(State),
    {ok, NewState, NewState}.


%% ===================================================================
%% Internal functions
%% ===================================================================

register_name({local, Name}) when is_atom(Name) ->
    erlang:register(Name, self()),
    ok;
register_name({global, Name}) ->
    global:register_name(Name, self()),
    ok.


do_init(Parent, Module, Args) ->
    Debug = sys:debug_options([]),

    case Module:init(Args) of
        {ok, State} ->
            Timeout = infinity;
        {ok, State, Timeout} ->
            ok
    end,

    ok = proc_lib:init_ack(Parent, {ok, self()}),
    loop(Parent, Debug, State, Module, Timeout).


loop(Parent, Debug, State, Module, Timeout) ->
    receive
        {system, From, Msg} ->
            sys:handle_system_msg(Msg, From, Parent, ?MODULE, Debug, [State, Module, Timeout]);
        Msg ->
            handle_msg(Parent, Debug, State, Module, Timeout, Msg)
    after
        Timeout ->
            handle_msg(Parent, Debug, State, Module, Timeout, timeout)
    end.


handle_msg(Parent, Debug, State, Module, Timeout, Msg) ->
    case catch Module:handle_msg(Msg, State) of
        {ok, NewState} ->
            loop(Parent, Debug, NewState, Module, infinity);
        {ok, NewState, Timeout} ->
            loop(Parent, Debug, NewState, Module, Timeout);
        {'EXIT', Reason} ->
            terminate(Reason, Module, State)
    end.


terminate(Reason, Module, State) ->
    ok = Module:terminate(Reason, State),
    erlang:exit(Reason).