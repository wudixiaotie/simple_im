-module(simple_im_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case application:get_env(simple_im, app_mode) of
        {ok, http} ->
            start_http_server();
        _ ->
            simple_im_sup:start_link()
    end.

stop(_State) ->
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================

start_http_server() ->
    ok = application:start(crypto),
    ok = application:start(cowlib),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/server/[...]", server_handler, []}
        ]}
    ]),
    DefaultHttpPort = env:get(http_port),
    {ok, Port} = utility:get_free_port(DefaultHttpPort),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    http_server_sup:start_link().