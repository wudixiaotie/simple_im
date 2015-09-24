-module (simple_im).

-export ([start/0, start/1]).


start() ->
    application:start(simple_im).

start(http) ->
    application:set_env(simple_im, app_mode, http),
    application:start(simple_im);
start(_) ->
    application:start(simple_im).