%% ===================================================================
%% Author xiaotie
%% 2015-10-23
%% handler helper
%% ===================================================================

-module(handler_helper).

-export([init/3, verify_token/1, return404/1, success/0, error/2,
         complete_notification/1, complete_group_notification/1,
         return/3, file_id/0, get_form_data/1]).



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
    Result = redis:q([<<"HMGET">>, TokenKey, <<"user_id">>, <<"device">>]),
    case Result of
        {ok, [undefined, undefined]} ->
            {ok, TomlBin} = ?MODULE:error(1, <<"Token error">>),
            {error, TomlBin};
        {ok, [UserIdBin, DeviceName]} ->
            UserId = erlang:binary_to_integer(UserIdBin),
            {ok, UserId, DeviceName}
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
    Timestamp = erlang:system_time(seconds),
    TimestampBin = erlang:integer_to_binary(Timestamp),
    NewAttrs = [{<<"id">>, <<"n_", TimestampBin/binary>>},
                {<<"ts">>, Timestamp}|Attrs],
    {ok, {<<"n">>, NewAttrs}}.


complete_group_notification(Attrs) ->
    Timestamp = erlang:system_time(seconds),
    TimestampBin = erlang:integer_to_binary(Timestamp),
    NewAttrs = [{<<"id">>, <<"gn_", TimestampBin/binary>>},
                {<<"ts">>, Timestamp}|Attrs],
    {ok, {<<"gn">>, NewAttrs}}.


return(Status, Bin, Req) ->
    cowboy_req:reply(Status, [{<<"content-type">>, <<"text/html">>}], Bin, Req).


file_id() ->
    {A, B, C} = os:timestamp(),
    Timestamp = (A * 1000000 + B) * 1000000 + C,
    TimestampBin = erlang:integer_to_binary(Timestamp),
    {ok, RandomNumber} = utility:random_number(88),
    RandomNumberBin = erlang:integer_to_binary(RandomNumber),
    FileId = <<TimestampBin/binary, "_", RandomNumberBin/binary>>,
    {ok, FileId}.


get_form_data(Req) ->
    get_form_data(Req, []).
get_form_data(Req, Result) ->
    case cowboy_req:part(Req) of
        {ok, Headers, Req2} ->
            case cow_multipart:form_data(Headers) of
                {data, Key} ->
                    {ok, Value, Req3} = cowboy_req:part_body(Req2);
                {file, Key, _Filename, _CType, _CTransferEncoding} ->
                    {ok, Value, Req3} = stream_file(Req2)
            end,
            get_form_data(Req3, [{Key, Value}|Result]);
        {done, Req2} ->
            {ok, Result, Req2}
    end.



%% ===================================================================
%% Internal functions
%% ===================================================================

stream_file(Req) ->
    stream_file(Req, <<>>).
stream_file(Req, Result) ->
    case cowboy_req:part_body(Req) of
        {ok, Data, Req2} ->
            {ok, <<Result/binary, Data/binary>>, Req2};
        {more, Data, Req2} ->
            stream_file(Req2, <<Result/binary, Data/binary>>)
    end.