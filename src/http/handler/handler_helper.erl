%% ===================================================================
%% Author xiaotie
%% 2015-10-23
%% handler helper
%% ===================================================================

-module(handler_helper).

-export([init/3, verify_token/1, return404/1]).



%% ===================================================================
%% API functions
%% ===================================================================

init(Module, Req, Opts) ->
    Path = cowboy_req:path_info(Req),
    Method = cowboy_req:method(Req),
    Req2 = Module:handle_request(Path, Method, Req),
    {ok, Req2, Opts}.


verify_token(Req) ->
    PostVals = cowboy_req:parse_cookies(Req),
    {<<"token">>, Token} = lists:keyfind(<<"token">>, 1, PostVals),
    {ok, TokenKey} = redis:key({token, Token}),
    Result = redis:q([<<"HGET">>, TokenKey, <<"user_id">>]),
    case Result of
        {ok, undefined} ->
            Toml = {<<"response">>, [{<<"status">>, 1}, {<<"r">>, <<"Token error">>}]},
            {ok, TomlBin} = toml:term_2_binary(Toml),
            {error, TomlBin};
        {ok, UserIdBin} ->
            UserId = erlang:binary_to_integer(UserIdBin),
            {ok, UserId}
    end.


return404(Req) ->
    Toml = {<<"response">>, [{<<"status">>, 404}]},
    {ok, TomlBin} = toml:term_2_binary(Toml),
    cowboy_req:reply(404, [], TomlBin, Req).