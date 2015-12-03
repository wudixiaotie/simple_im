%% ===================================================================
%% Author xiaotie
%% 2015-9-24
%% server handler
%% ===================================================================

-module(upload_handler).

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
        {ok, UserId} ->
            {ok, Headers, Req2} = cowboy_req:part(Req),
            {ok, Data, Req3} = cowboy_req:part_body(Req2),
            {ok, RandomBin} = utility:random_binary_16(),
            FileId = <<"audio_", RandomBin/binary>>,
            {ok, <<"OK">>} = redis:q([<<"SET">>, FileId, Data]),
            {ok, TomlBin1} = handler_helper:success(),
            TomlBin2 = <<" file_id = \"", FileId, "\"">>,
            handler_helper:return(200, <<TomlBin1/binary, TomlBin2/binary>>, Req3)
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