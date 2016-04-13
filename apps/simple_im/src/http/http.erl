%% ===================================================================
%% Author xiaotie
%% 2016-4-13
%% http server application
%% ===================================================================

-module(http).

% APIs
-export([start_link/0]).



%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    application:start(crypto),
    application:start(cowlib),
    application:start(ranch),
    application:start(cowboy),

    RoutePath = http_route:path(),
    Dispatch = cowboy_router:compile(RoutePath),
    DefaultHttpPort = env:get(http_port),
    {ok, Port} = utility:free_port(DefaultHttpPort),
    {ok, SslConfigs} = utility:ssl_configs(),
    {ok, Pid} = cowboy:start_https(https, 2, [{port, Port}] ++ SslConfigs, [{env, [{dispatch, Dispatch}]}]),

    log:i("[Http] Server with SSL/TLS encryption start to listen port: ~p~n", [Port]),
    true = erlang:link(Pid),
    {ok, Pid}.



%% ===================================================================
%% Internal functions
%% ===================================================================