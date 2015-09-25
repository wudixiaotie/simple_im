%% ===================================================================
%% Author xiaotie
%% 2015-9-24
%% server handler
%% ===================================================================

-module (server_handler).

-export([init/2]).

-include("user.hrl").



init(Req, Opts) ->
    Path = cowboy_req:path_info(Req),
    Method = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req),
    Req2 = handle_request(Path, Method, HasBody, Req),
    {ok, Req2, Opts}.



%% ===================================================================
%% Internal functions
%% ===================================================================

handle_request([<<"login">>], <<"POST">>, true, Req) ->
    {ok, PostVals, _} = cowboy_req:body_qs(Req),
    {ok, User} = users:parse(PostVals),
    Toml = case users:verify(User#user.phone, User#user.password) of
        {ok, true, UserId} ->
            {ok, Token} = utility:guid(),
            [Ip, Port] = get_node(),
            redis:q([<<"HMSET">>, token_key(Token), <<"ip">>, Ip, <<"port">>, Port]),
            {<<"response">>, [{<<"status">>, 0},
                              {<<"server">>, Ip},
                              {<<"port">>, Port},
                              {<<"user">>, [{<<"token">>, Token},
                                            {<<"id">>, UserId}]}]};
        {ok, false} ->
            {<<"response">>, [{<<"status">>, 1}]};
        {error, _Reason} ->
            {<<"response">>, [{<<"status">>, 2}]}
    end,
    {ok, TomlBin} = utility:tuple_to_toml(Toml),
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request([<<"reconnect">>], <<"POST">>, true, Req) ->
    {ok, PostVals, _} = cowboy_req:body_qs(Req),
    {ok, User} = users:parse(PostVals),
    Result = redis:q([<<"HMGET">>, token_key(User#user.token), <<"ip">>, <<"port">>]),
    Toml = case Result of
        {ok, [undefined, undefined]} ->
            {<<"response">>, [{<<"status">>, 1}]};
        {ok, [Ip, Port]} ->
            {<<"response">>, [{<<"status">>, 0},
                              {<<"server">>, Ip},
                              {<<"port">>, Port}]};
        _ ->
            {<<"response">>, [{<<"status">>, 2}]}
    end,
    {ok, TomlBin} = utility:tuple_to_toml(Toml),
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request(_, _, _, Req) ->
    Toml = {<<"response">>, [{<<"status">>, 404}]},
    {ok, TomlBin} = utility:tuple_to_toml(Toml),
    cowboy_req:reply(404, [], TomlBin, Req).


token_key(Token) ->
    <<"client_", Token/binary>>.


get_node() ->
    [<<"192.168.1.137">>, <<"1987">>].