%% ===================================================================
%% Author xiaotie
%% 2015-09-24
%% group message handler
%% ===================================================================

-module(gm_handler).

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
    process(<<"audio">>, FileDir, <<".amr">>, Req);
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
        {ok, UserId, DeviceName} ->
            {ok, PostVals, Req2} = handler_helper:get_form_data(Req),
            case utility:check_parameters([Type, <<"g_id">>], PostVals) of
                {ok, [Data, GroupIdBin]} ->
                    {ok, FileId} = handler_helper:file_id(),
                    FileName = <<FileId/binary, Extension/binary>>,
                    FilePath = <<FileDir/binary, FileName/binary>>,
                    ok = file:write_file(FilePath, Data),
                    Url = <<"file/", Type/binary, "/", FileName/binary>>,
                    GroupId = erlang:binary_to_integer(GroupIdBin),
                    M = {<<"gm">>, [{<<"c">>, Url},
                                    {<<"from">>, UserId},
                                    {<<"g_id">>, GroupId},
                                    {<<"ts">>, erlang:system_time(seconds)},
                                    {<<"t">>, Type},
                                    {<<"d">>, DeviceName},
                                    {<<"id">>, <<"gm_", FileId/binary>>}]},
                    {ok, MBin} = toml:term_2_binary(M),
                    ok = agent:notify(MBin),

                    {ok, TomlBin} = handler_helper:success();
                {error, Reason} ->
                    {ok, TomlBin} = handler_helper:error(3, Reason)
            end,
            handler_helper:return(200, TomlBin, Req2)
    end.