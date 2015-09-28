%% ===================================================================
%% Author xiaotie
%% 2015-9-24
%% server handler
%% ===================================================================

-module (server_handler).

-export([init/2]).

-include("user.hrl").



%% ===================================================================
%% API functions
%% ===================================================================

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
            [IP, Port] = get_node(),
            {ok, <<"OK">>} = redis:q([<<"HMSET">>, redis:key({token, Token}),
                                      <<"ip">>, IP, <<"port">>, Port,
                                      <<"user_id">>, UserId]),
            {<<"response">>, [{<<"status">>, 0},
                              {<<"server">>, IP},
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
    UserIdBin = utility:int_2_bin_str(User#user.id),
    Result = redis:q([<<"HMGET">>, redis:key({token, User#user.token}),
                      <<"ip">>, <<"port">>, <<"user_id">>]),
    Toml = case Result of
        {ok, [undefined, undefined, undefined]} ->
            {<<"response">>, [{<<"status">>, 1}]};
        {ok, [IP, Port, UserIdBin]} ->
            {<<"response">>, [{<<"status">>, 0},
                              {<<"server">>, IP},
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


get_node() ->
    [<<"192.168.1.137">>, <<"1987">>].