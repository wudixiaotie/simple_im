-module(simple_im).

-export([start/0, start/1, stop/0]).



%% ===================================================================
%% API functions
%% ===================================================================

start() ->
    start(im).


start(im) ->
    application:set_env(simple_im, app_mode, im),
    application:start(simple_im);
start(http) ->
    application:set_env(simple_im, app_mode, http),
    application:start(simple_im);
start(middleman) ->
    application:set_env(simple_im, app_mode, middleman),
    application:start(simple_im).


stop() ->
    application:stop(simple_im).