%% ===================================================================
%% Author xiaotie
%% 2015-9-24
%% health handler
%% ===================================================================

-module (health_handler).

-export([init/2]).

init(Req, Opts) ->
    Req2 = cowboy_req:reply(200, [], <<"I'm OK! :)\r\n">>, Req),
    {ok, Req2, Opts}.