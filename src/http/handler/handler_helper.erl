%% ===================================================================
%% Author xiaotie
%% 2015-10-23
%% handler helper
%% ===================================================================

-module(handler_helper).

-export([init/3, verify_token/1, return404/1, success/0, error/2,
         complete_notification/1, return/3]).



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
            {ok, TomlBin} = ?MODULE:error(1, <<"Token error">>),
            {error, TomlBin};
        {ok, UserIdBin} ->
            UserId = erlang:binary_to_integer(UserIdBin),
            {ok, UserId}
    end.


return404(Req) ->
    TomlBin = <<"[[response]] status = 404">>,
    return(404, TomlBin, Req).


success() ->
    {ok, <<"[[response]] status = 0">>}.


error(Status, Reason) ->
    Toml = {<<"response">>, [{<<"status">>, Status},
                             {<<"reason">>, Reason}]},
    {ok, TomlBin} = toml:term_2_binary(Toml),
    {ok, TomlBin}.


complete_notification(Attrs) ->
    Timestamp = utility:timestamp(),
    TimestampBin = erlang:integer_to_binary(Timestamp),
    NewAttrs = [{<<"id">>, <<"n_", TimestampBin/binary>>},
                {<<"ts">>, Timestamp}|Attrs],
    {ok, {<<"n">>, NewAttrs}}.


return(Status, Bin, Req) ->
    cowboy_req:reply(Status, [{<<"content-type">>, <<"text/html">>}], Bin, Req).