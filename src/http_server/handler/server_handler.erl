%% ===================================================================
%% Author xiaotie
%% 2015-9-24
%% server handler
%% ===================================================================

-module (server_handler).

-export([init/2]).

init(Req, Opts) ->
    Path = cowboy_req:path_info(Req),
    Method = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req),
    Req2 = handle_request(Path, Method, HasBody, Req),
    {ok, Req2, Opts}.



%% ===================================================================
%% Internal functions
%% ===================================================================

handle_request([<<"login">>], <<"GET">>, false, Req) ->
    Toml = {<<"response">>, [{<<"status">>, 0},
                             {<<"server">>, <<"192.168.1.137">>},
                             {<<"port">>, <<"1987">>}]},
    {ok, TomlBin} = utility:tuple_to_toml(Toml),
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request([<<"reconnect">>], <<"POST">>, true, Req) ->
    {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
    {<<"token">>, Token} = lists:keyfind(<<"token">>, 1, PostVals),
    Toml = {<<"response">>, [{<<"status">>, 0},
                             {<<"server">>, <<"192.168.1.137">>},
                             {<<"port">>, <<"1987">>}]},
    {ok, TomlBin} = utility:tuple_to_toml(Toml),
    cowboy_req:reply(200, [], Toml, Req);
handle_request(_, _, _, Req) ->
    Toml = {<<"response">>, [{<<"status">>, 1}]},
    {ok, TomlBin} = utility:tuple_to_toml(Toml),
    cowboy_req:reply(404, [], TomlBin, Req).