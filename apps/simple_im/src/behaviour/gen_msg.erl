-module(gen_msg).

% APIs
-export([start_link/3, init/3, start_link/4, init/4, enter_loop/2, enter_loop/3]).

% system message
-export([system_continue/3, system_terminate/4, system_get_state/1,
         system_replace_state/2, format_status/2]).

-define(TIMEOUT, 5000).



%% ===================================================================
%% APIs
%% ===================================================================

-callback init(Args :: term()) -> {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate}.
-callback handle_msg(Msg :: term(), State :: term()) -> {ok, NewState :: term()} | {ok, State :: term(), timeout() | hibernate}.
-callback terminate(Reason :: term(), State :: term()) -> ok.

start_link(Module, Args, Opts) ->
    proc_lib:start_link(?MODULE, init, [self(), Module, Args], ?TIMEOUT, Opts).


start_link(Name, Module, Args, Opts) ->
    proc_lib:start_link(?MODULE, init, [self(), Name, Module, Args], ?TIMEOUT, Opts).


init(Parent, Module, Args) ->
    do_init(Parent, Module, Args).


init(Parent, Name, Module, Args) ->
    ok = register_name(Name),
    do_init(Parent, Module, Args).


enter_loop(Module, State) ->
    enter_loop(Module, State, infinity).


enter_loop(Module, State, Timeout) ->
    Parent = get_parent(),
    Debug = sys:debug_options([]),
    loop(Parent, Debug, Module, State, Timeout).



%% ===================================================================
%% main loop function
%% ===================================================================

do_init(Parent, Module, Args) ->
    Debug = sys:debug_options([]),

    % update process dictionary, so otp system can get real initial call
    % not gen_msg:init/1. Also observer will get behaviour about this process
    % instead of undefined.
    erlang:put('$initial_call', {Module, init, 1}),

    case Module:init(Args) of
        {ok, State} ->
            Timeout = infinity;
        {ok, State, Timeout} ->
            ok
    end,

    ok = proc_lib:init_ack(Parent, {ok, self()}),
    loop(Parent, Debug, Module, State, Timeout).


loop(Parent, Debug, Module, State, Timeout) ->
    receive
        {system, From, Msg} ->
            sys:handle_system_msg(Msg, From, Parent, ?MODULE, Debug, [Module, State, Timeout]);
        Msg ->
            do_loop(Parent, Debug, Module, State, Msg)
    after
        Timeout ->
            do_loop(Parent, Debug, Module, State, timeout)
    end.


do_loop(Parent, Debug, Module, State, Msg) ->
    case catch Module:handle_msg(Msg, State) of
        {ok, NewState} ->
            loop(Parent, Debug, Module, NewState, infinity);
        {ok, NewState, NewTimeout} ->
            loop(Parent, Debug, Module, NewState, NewTimeout);
        {stop, Reason, NewState} ->
            terminate(Reason, Module, NewState);
        {'EXIT', Reason} ->
            terminate(Reason, Module, State);
        Other ->
            terminate({bad_return_value, Other}, Module, State)
    end.


terminate(Reason, Module, State) ->
    ok = Module:terminate(Reason, State),
    erlang:exit(Reason).



%% ===================================================================
%% system message
%% ===================================================================

system_continue(Parent, Debug, [Module, State, Timeout]) ->
    loop(Parent, Debug, Module, State, Timeout).


system_terminate(Reason, _Parent, _Debug, [Module, State, _Timeout]) ->
    terminate(Reason, Module, State).


system_get_state([State, _Module, _Timeout]) ->
    {ok, State, State}.


system_replace_state(StateFun, State) ->
    NewState = StateFun(State),
    {ok, NewState, NewState}.


format_status(_Opt, StatusData) ->
    [_PDict, SysState, Parent, Debug, [_Module, State, _Timeout]] = StatusData,
    case erlang:process_info(self(), registered_name) of
        {registered_name, Name} ->
            ok;
        _ ->
            Name = self()
    end,
    Header = gen:format_status_header("Status for generic msg server", Name),
    Log = sys:get_debug(log, Debug, []),
    Specfic = [{data, [{"State", State}]}],

    [{header, Header},
     {data, [{"Status", SysState},
         {"Parent", Parent},
         {"Logged events", Log}]} |
     Specfic].



%% ===================================================================
%% Internal functions
%% ===================================================================

register_name({local, Name}) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            erlang:register(Name, self()),
            ok;
        _ ->
            erlang:error("register name exist!")
    end;
register_name({global, Name}) ->
    global:register_name(Name, self()),
    ok.


get_parent() ->
    case get('$ancestors') of
        [Parent | _] when is_pid(Parent)->
            Parent;
        [Parent | _] when is_atom(Parent)->
            name_to_pid(Parent);
        _ ->
            exit(process_was_not_started_by_proc_lib)
    end.


name_to_pid(Name) ->
    case whereis(Name) of
        undefined ->
            case global:whereis_name(Name) of
                undefined ->
                    exit(could_not_find_registered_name);
                Pid ->
                    Pid
                end;
        Pid ->
            Pid
    end.