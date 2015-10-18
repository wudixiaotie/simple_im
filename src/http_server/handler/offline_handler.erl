%% ===================================================================
%% Author xiaotie
%% 2015-10-5
%% offline handler
%% ===================================================================

-module (offline_handler).

-export([init/2]).



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

handle_request([<<"get">>], <<"POST">>, true, Req) ->
    {ok, PostVals, _} = cowboy_req:body_qs(Req),
    {<<"token">>, Token} = lists:keyfind(<<"token">>, 1, PostVals),
    {ok, TokenKey} = redis:key({token, Token}),
    Result = redis:q([<<"HGET">>, TokenKey, <<"user_id">>]),
    case Result of
        {ok, undefined} ->
            Toml = {<<"response">>, [{<<"status">>, 1}, {<<"r">>, <<"Token error">>}]},
            {ok, TomlBin} = toml:term_2_binary(Toml);
        {ok, UserIdBin} ->
            UserId = erlang:binary_to_integer(UserIdBin),
            {ok, MsgList} = offline:get(UserId),
            Toml = {<<"response">>, [{<<"status">>, 0}]},
            {ok, TomlBin1} = toml:term_2_binary(Toml),
            {ok, TomlBin2} = list_2_binary(MsgList),
            TomlBin = <<TomlBin1/binary, "\r\n", TomlBin2/binary>>
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request([<<"clean">>], <<"POST">>, true, Req) ->
    {ok, PostVals, _} = cowboy_req:body_qs(Req),
    {<<"token">>, Token} = lists:keyfind(<<"token">>, 1, PostVals),
    {ok, TokenKey} = redis:key({token, Token}),
    Result = redis:q([<<"HGET">>, TokenKey, <<"user_id">>]),
    case Result of
        {ok, undefined} ->
            Toml = {<<"response">>, [{<<"status">>, 1}, {<<"r">>, <<"Token error">>}]},
            {ok, TomlBin} = toml:term_2_binary(Toml);
        {ok, UserIdBin} ->
            UserId = erlang:binary_to_integer(UserIdBin),
            ok = offline:clean(UserId),
            Toml = {<<"response">>, [{<<"status">>, 0}]},
            {ok, TomlBin} = toml:term_2_binary(Toml)
    end,
    cowboy_req:reply(200, [], TomlBin, Req).


list_2_binary(MsgList) ->
    list_2_binary(MsgList, <<>>).
list_2_binary([H|T], Result) ->
    list_2_binary(T, <<H/binary, "\r\n", Result/binary>>);
list_2_binary([], Result) ->
    {ok, Result}.