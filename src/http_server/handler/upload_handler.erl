%% ===================================================================
%% Author xiaotie
%% 2015-9-24
%% server handler
%% ===================================================================

-module (upload_handler).

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
            cowboy_req:reply(200, [], TomlBin, Req);
        {ok, UserId} ->
            {ok, Headers, Req2} = cowboy_req:part(Req),
            {ok, Data, Req3} = cowboy_req:part_body(Req2),
            {ok, RandomBin} = utility:random_binary_16(),
            FileId = <<"audio_", RandomBin/binary>>,
            {ok, <<"OK">>} = redis:q([<<"SET">>, FileId, Data]),
            cowboy_req:reply(200, [], FileId, Req3)
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