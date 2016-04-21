-module(simple_im).

-export([start/1, stop/0]).

-include("simple_im.hrl").



%% ===================================================================
%% API functions
%% ===================================================================

start(im) ->
    application:set_env(?APPLICATION_NAME, app_mode, im),
    application:start(?APPLICATION_NAME);
start(http) ->
    application:set_env(?APPLICATION_NAME, app_mode, http),
    application:start(?APPLICATION_NAME);
start(session_server) ->
    application:set_env(?APPLICATION_NAME, app_mode, session_server),
    application:start(?APPLICATION_NAME);
start(middleman) ->
    application:set_env(?APPLICATION_NAME, app_mode, middleman),
    application:start(?APPLICATION_NAME).


stop() ->
    application:stop(?APPLICATION_NAME).