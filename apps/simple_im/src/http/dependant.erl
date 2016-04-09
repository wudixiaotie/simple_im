%% ===================================================================
%% Author xiaotie
%% 2015-9-20
%% dependent application
%% ===================================================================

-module(dependant).

% APIs
-export([start_link/1]).



%% ===================================================================
%% APIs
%% ===================================================================

start_link(ranch) ->
    {ok, Pid} = start_app(ranch, ranch_sup),
    {ok, Pid};
start_link(cowboy_app) ->
    application:start(crypto),
    application:start(cowlib),
    {ok, Pid} = start_app(cowboy, cowboy_sup),
    {ok, Pid};
start_link(cowboy_http) ->
    RoutePath = http_route:path(),
    Dispatch = cowboy_router:compile(RoutePath),
    DefaultHttpPort = env:get(http_port),
    {ok, Port} = utility:free_port(DefaultHttpPort),

    {ok, SslConfigs} = utility:ssl_configs(),
    {ok, Pid} = cowboy:start_https(https, 100, [{port, Port}] ++ SslConfigs, [{env, [{dispatch, Dispatch}]}]),

    log:i("[Http] Server with SSL/TLS encryption start to listen port: ~p~n", [Port]),
    true = erlang:link(Pid),
    {ok, Pid}.


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

start_app(AppName, KeyProcessName) ->
    case application:start(AppName) of
        ok ->
            case erlang:whereis(KeyProcessName) of
                undefined ->
                    {error, key_process_down};
                KeyProcessPid ->
                    true = erlang:link(KeyProcessPid),
                    {ok, KeyProcessPid}
            end;
        {error, {already_started, AppName}} ->
            case erlang:whereis(KeyProcessName) of
                undefined ->
                    ok;
                KeyProcessPid ->
                    true = erlang:exit(KeyProcessPid, kill)
            end,
            timer:sleep(1000),
            start_app(AppName, KeyProcessName)
    end.