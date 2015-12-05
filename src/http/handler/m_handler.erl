%% ===================================================================
%% Author xiaotie
%% 2015-9-24
%% message handler
%% ===================================================================

-module(m_handler).

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
    FileDir = erlang:list_to_binary(env:get(audio_dir)),
    process(<<"audio">>, FileDir, <<".mp4">>, Req);
handle_request([<<"video">>], <<"POST">>, Req) ->
    FileDir = erlang:list_to_binary(env:get(video_dir)),
    process(<<"video">>, FileDir, <<".mp4">>, Req);
handle_request([<<"image">>], <<"POST">>, Req) ->
    FileDir = erlang:list_to_binary(env:get(image_dir)),
    process(<<"image">>, FileDir, <<".png">>, Req);
handle_request(_, _, Req) ->
    handler_helper:return404(Req).



%% ===================================================================
%% Internal functions
%% ===================================================================

process(Type, FileDir, Extension, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            handler_helper:return(200, TomlBin, Req);
        {ok, UserId} ->
            {ok, PostVals, Req2} = handler_helper:get_form_data(Req),
            case utility:check_parameters([Type, <<"to">>], PostVals) of
                {ok, [Data, ToUserIdBin]} ->
                    {ok, FileId} = handler_helper:file_id(),
                    FileName = <<FileId/binary, Extension/binary>>,
                    FilePath = <<FileDir/binary, FileName/binary>>,
                    ok = file:write_file(FilePath, Data),
                    Url = <<"file/video/", FileName/binary>>,
                    ToUserId = erlang:binary_to_integer(ToUserIdBin),
                    M = {<<"m">>, [{<<"c">>, Url},
                                   {<<"from">>, UserId},
                                   {<<"to">>, ToUserId},
                                   {<<"timestamp">>, utility:timestamp()},
                                   {<<"t">>, Type},
                                   {<<"id">>, <<"m_", FileId/binary>>}]},
                    {ok, MBin} = toml:term_2_binary(M),
                    ok = agent:offer_a_reward(MBin),

                    {ok, TomlBin} = handler_helper:success();
                {error, Reason} ->
                    {ok, TomlBin} = handler_helper:error(3, Reason)
            end,
            handler_helper:return(200, TomlBin, Req2)
    end.