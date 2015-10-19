%% ===================================================================
%% Author xiaotie
%% 2015-9-14
%% dependent application
%% ===================================================================

-module (deps_monitor).

% APIs
-export([start_link/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {deps}).



%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, RanchRef} = start_dep(ranch),
    {ok, CowBoyAppRef} = start_dep(cowboy_app),
    {ok, CowBoyHttpRef} = start_dep(cowboy_http),
    State = #state{deps = [{ranch, RanchRef},
                           {cowboy_app, CowBoyAppRef},
                           {cowboy_http, CowBoyHttpRef}]},
    {ok, State}.


handle_call(_Request, _From, State) -> {reply, nomatch, State}.
handle_cast(_Msg, State) -> {noreply, State}.


handle_info({'DOWN', Ref, process, _, Info}, #state{deps = Deps} = State) ->
    NewState = case lists:keyfind(Ref, 2, Deps) of
        {DepName, Ref} ->
            log:e("~p is down: ~p, restarting!~n", [DepName, Info]),
            {ok, NewRef} = start_dep(DepName),
            NewDeps = lists:keyreplace(Ref, 2, Deps, {DepName, NewRef}),
            State#state{deps = NewDeps};
        _ ->
            State
    end,
    {noreply, NewState};
handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================

start_dep(ranch) ->
    ok = start_app(ranch),
    Ref = erlang:monitor(process, ranch_sup),
    {ok, Ref};
start_dep(cowboy_app) ->
    application:start(crypto),
    application:start(cowlib),
    ok = start_app(cowboy),
    Ref = erlang:monitor(process, cowboy_sup),
    {ok, Ref};
start_dep(cowboy_http) ->
    RoutePath = route:path(),
    Dispatch = cowboy_router:compile(RoutePath),
    DefaultHttpPort = env:get(http_port),
    {ok, Port} = utility:free_port(DefaultHttpPort),
    {ok, Pid} = cowboy:start_http(http, 100, [{port, Port}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    log:i("Http server start listen port: ~p~n", [Port]),
    Ref = erlang:monitor(process, Pid),
    {ok, Ref}.


start_app(Name) ->
    case application:start(Name) of
        ok ->
            ok;
        {error, {already_started, Name}} ->
            timer:sleep(1000),
            start_app(Name)
    end.