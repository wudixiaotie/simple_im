-module(simple_im).

-export([start/1, stop/0]).



%% ===================================================================
%% API functions
%% ===================================================================

start(im) ->
    application:set_env(?MODULE, app_mode, im),
    application:start(?MODULE);
start(http) ->
    application:set_env(?MODULE, app_mode, http),
    application:start(?MODULE);
start(middleman) ->
    application:set_env(?MODULE, app_mode, middleman),
    application:start(?MODULE).


stop() ->
    application:stop(?MODULE).