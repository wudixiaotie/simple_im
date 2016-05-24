%% ===================================================================
%% Author xiaotie
%% 2015-10-23
%% contact handler
%% ===================================================================

-module(contact_handler).

-export([init/2, handle_request/3]).



%% ===================================================================
%% API functions
%% ===================================================================

init(Req, Opts) ->
    handler_helper:init(?MODULE, Req, Opts).



%% ===================================================================
%% Request handler
%% ===================================================================

handle_request([<<"version">>, ContactVersionBin], <<"GET">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, UserId, _} ->
            ContactVersion = erlang:binary_to_integer(ContactVersionBin),
            {ok, Toml2} = contacts:find(UserId, ContactVersion),
            Toml1 = {<<"response">>, [{<<"status">>, 0}]},
            {ok, TomlBin1} = toml:term_2_binary(Toml1),
            {ok, TomlBin2} = toml:term_2_binary(Toml2),
            TomlBin = <<TomlBin1/binary, "\r\n", TomlBin2/binary>>
    end,
    handler_helper:return(200, TomlBin, Req);
handle_request([ToUserIdBin], <<"POST">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, UserId, DeviceName} ->
            {ok, PostVals, _} = cowboy_req:body_qs(Req),
            case utility:check_parameters([<<"ask">>], PostVals) of
                {ok, [Ask]} ->
                    ToUserId = erlang:binary_to_integer(ToUserIdBin),
                    case pre_contacts:create(UserId, ToUserId, Ask) of
                        {ok, 0} ->
                            Attrs = [{<<"t">>, <<"add_contact">>},
                                     {<<"from">>, UserId},
                                     {<<"to">>, ToUserId},
                                     {<<"ask">>, Ask},
                                     {<<"d">>, DeviceName}],
                            {ok, N} = handler_helper:complete_notification(Attrs),
                            {ok, NBin} = toml:term_2_binary(N),
                            ok = agent:notify(NBin),
                            {ok, TomlBin} = handler_helper:success();
                        {ok, 1} ->
                            {ok, TomlBin} = handler_helper:error(1, <<"Contact exists">>);
                        {ok, 2} ->
                            {ok, TomlBin} = handler_helper:error(1, <<"Waiting for accept">>);
                        {ok, _} ->
                            {ok, TomlBin} = handler_helper:error(1, <<"Unkonw Error">>)
                    end;
                {error, Reason} ->
                    {ok, TomlBin} = handler_helper:error(3, Reason)
            end
    end,
    handler_helper:return(200, TomlBin, Req);
handle_request([AUserIdBin], <<"PUT">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, BUserId, DeviceName} ->
            AUserId = erlang:binary_to_integer(AUserIdBin),
            case contacts:create(AUserId, BUserId) of
                {ok, 0} ->
                    Attrs = [{<<"t">>, <<"accept_contact">>},
                             {<<"from">>, BUserId},
                             {<<"to">>, AUserId},
                             {<<"d">>, DeviceName}],
                    {ok, N} = handler_helper:complete_notification(Attrs),
                    {ok, NBin} = toml:term_2_binary(N),
                    ok = agent:notify(NBin),
                    {ok, TomlBin} = handler_helper:success();
                {ok, 1} ->
                    {ok, TomlBin} = handler_helper:error(1, <<"Unauthorized operate">>);
                {ok, _} ->
                    {ok, TomlBin} = handler_helper:error(1, <<"Unkonw Error">>)
            end
    end,
    handler_helper:return(200, TomlBin, Req);
handle_request([ToUserIdBin], <<"DELETE">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, UserId, DeviceName} ->
            ToUserId = erlang:binary_to_integer(ToUserIdBin),
            ok = contacts:delete(UserId, ToUserId),
            Attrs = [{<<"t">>, <<"delete_contact">>},
                     {<<"from">>, UserId},
                     {<<"to">>, ToUserId},
                     {<<"d">>, DeviceName}],
            {ok, N} = handler_helper:complete_notification(Attrs),
            {ok, NBin} = toml:term_2_binary(N),
            ok = agent:notify(NBin),
            {ok, TomlBin} = handler_helper:success()
    end,
    handler_helper:return(200, TomlBin, Req);
handle_request(_, _, Req) ->
    handler_helper:return404(Req).



%% ===================================================================
%% Internal functions
%% ===================================================================