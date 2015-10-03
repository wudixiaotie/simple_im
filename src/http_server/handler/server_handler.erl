%% ===================================================================
%% Author xiaotie
%% 2015-9-24
%% server handler
%% ===================================================================

-module (server_handler).

-export([init/2]).

-include("user.hrl").

-compile (export_all).



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
            TokenKey = redis:key({token, Token}),
            {ok, <<"OK">>} = redis:q([<<"HMSET">>, TokenKey,
                                      <<"ip">>, IP, <<"port">>, Port,
                                      <<"user_id">>, UserId]),
            {ok, <<"1">>} = redis:q([<<"EXPIRE">>, TokenKey, 100]),
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
    UserIdBin = erlang:integer_to_binary(User#user.id),
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
handle_request([<<"failed">>], <<"POST">>, true, Req) ->
    {ok, PostVals, _} = cowboy_req:body_qs(Req),
    {<<"token">>, Token} = lists:keyfind(<<"token">>, 1, PostVals),
    Result = redis:q([<<"HMGET">>, redis:key({token, Token}),
                      <<"ip">>, <<"port">>]),
    Toml = case Result of
        {ok, [undefined, undefined]} ->
            {<<"response">>, [{<<"status">>, 1}]};
        {ok, [IP, Port]} ->
            case gen_tcp:connect(erlang:binary_to_list(IP),
                                 erlang:binary_to_integer(Port), []) of
                {ok, Socket} ->
                    gen_tcp:close(Socket),
                    {<<"response">>, [{<<"status">>, 3},
                                      {<<"r">>, <<"IM is online">>}]};
                {error, _Reason} ->
                    redis:q([<<"HDEL">>, redis:key(im_list), utility:ip_port(IP, Port)]),
                    [NewIP, NewPort] = get_node(),
                    redis:q([<<"HMSET">>, redis:key({token, Token}),
                             <<"ip">>, NewIP, <<"port">>, NewPort]),
                    {<<"response">>, [{<<"status">>, 0},
                                      {<<"server">>, NewIP},
                                      {<<"port">>, NewPort}]};
                _ ->
                    {<<"response">>, [{<<"status">>, 2}]}
            end;
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
    {ok, IMList} = redis:q([<<"HKEYS">>, <<"im_list">>]),
    {ok, Index} = utility:random_number(erlang:length(IMList)),
    IM = lists:nth(Index, IMList),
    IPPort = erlang:binary_to_list(IM),
    re:split(IPPort, ":").