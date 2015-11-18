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
            http_sup:start_link();
        _ ->
            simple_im_sup:start_link()
    end.

stop(_State) ->
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================