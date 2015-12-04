%% ===================================================================
%% Author xiaotie
%% 2015-9-24
%% file handler
%% ===================================================================

-module(file_handler).

-export([init/2, handle_request/3]).



%% ===================================================================
%% API functions
%% ===================================================================

init(Req, Opts) ->
    handler_helper:init(?MODULE, Req, Opts).



%% ===================================================================
%% Request handler
%% ===================================================================

handle_request([<<"audio">>], <<"POST">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            handler_helper:return(200, TomlBin, Req);
        {ok, _} ->
            {ok, _, Req2} = cowboy_req:part(Req),
            {ok, Data, Req3} = cowboy_req:part_body(Req2),
            {ok, FileId} = file_id(),
            FileDir = erlang:list_to_binary(env:get(file_dir)),
            FilePath = <<FileDir/binary, FileId/binary, ".amr">>,
            ok = file:write_file(FilePath, Data),
            {ok, <<"OK">>} = redis:q([<<"SET">>, FileId, FilePath]),
            {ok, TomlBin1} = handler_helper:success(),
            TomlBin2 = <<" file_id = \"", FileId/binary, "\"">>,
            handler_helper:return(200, <<TomlBin1/binary, TomlBin2/binary>>, Req3)
    end;
handle_request([<<"audio">>, FileId], <<"GET">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            handler_helper:return(200, TomlBin, Req);
        {ok, _} ->
            case redis:q([<<"GET">>, FileId]) of
                {ok, undefined} ->
                    {ok, TomlBin} = handler_helper:error(1, <<"File does not exist!">>),
                    handler_helper:return(200, TomlBin, Req);
                {ok, FilePath} ->
                    handler_helper:return(200, TomlBin, Req)
            end
    end;
handle_request([<<"video">>], <<"POST">>, Req) ->
    ok;
handle_request([<<"image">>], <<"POST">>, Req) ->
    ok;
handle_request(_, _, Req) ->
    handler_helper:return404(Req).



%% ===================================================================
%% Internal functions
%% ===================================================================

file_id() ->
    {A, B, C} = os:timestamp(),
    Timestamp = (A * 1000000 + B) * 1000000 + C,
    TimestampBin = erlang:integer_to_binary(Timestamp),
    {ok, RandomNumber} = utility:random_number(88),
    RandomNumberBin = erlang:integer_to_binary(RandomNumber),
    FileId = <<TimestampBin/binary, "_", RandomNumberBin/binary>>,
    {ok, FileId}.