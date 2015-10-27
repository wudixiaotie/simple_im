%% ===================================================================
%% Author xiaotie
%% 2015-10-23
%% contact handler
%% ===================================================================

-module (contact_handler).

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
        {ok, UserId} ->
            ContactVersion = erlang:binary_to_integer(ContactVersionBin),
            {ok, ContactList} = contacts:find(UserId, ContactVersion),
            {ok, Toml2} = users:to_toml(ContactList),
            Toml1 = {<<"response">>, [{<<"status">>, 0}]},
            {ok, TomlBin1} = toml:term_2_binary(Toml1),
            {ok, TomlBin2} = toml:term_2_binary(Toml2),
            TomlBin = <<TomlBin1/binary, "\r\n", TomlBin2/binary>>
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request([ContactIdBin], <<"DELETE">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, UserId} ->
            ContactId = erlang:binary_to_integer(ContactIdBin),
            {ok, [UserNewVersion, ContactNewVersion]} = contacts:delete(UserId, ContactId),
            Toml1 = {<<"response">>, [{<<"status">>, 0}]},
            Toml2 = [{<<"user">>, [{<<"id">>, UserId},
                                   {<<"contact_version">>, UserNewVersion}]},
                     {<<"user">>, [{<<"id">>, ContactId},
                                   {<<"contact_version">>, ContactNewVersion}]}],
            {ok, TomlBin1} = toml:term_2_binary(Toml1),
            {ok, TomlBin2} = toml:term_2_binary(Toml2),
            TomlBin = <<TomlBin1/binary, "\r\n", TomlBin2/binary>>
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request([ContactIdBin], <<"POST">>, Req) ->
    {ok, PostVals, _} = cowboy_req:body_qs(Req),
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, UserId} ->
            ContactId = erlang:binary_to_integer(ContactIdBin),
            {<<"message">>, Message} = lists:keyfind(<<"message">>, 1, PostVals),
            case pre_contacts:create(UserId, ContactId, Message) of
                {ok, 0} ->
                    Response = {<<"response">>, [{<<"status">>, 0}]},
                    {ok, TomlBin} = toml:term_2_binary(Response);
                {ok, 1} ->
                    Response = {<<"response">>, [{<<"status">>, 1},
                                                 {<<"reason">>, <<"Contact exists">>}]},
                    {ok, TomlBin} = toml:term_2_binary(Response);
                {ok, 2} ->
                    Response = {<<"response">>, [{<<"status">>, 2},
                                                 {<<"reason">>, <<"Waiting for accept">>}]},
                    {ok, TomlBin} = toml:term_2_binary(Response);
                {ok, _} ->
                    Response = {<<"response">>, [{<<"status">>, 3},
                                                 {<<"reason">>, <<"Unkonw Error">>}]},
                    {ok, TomlBin} = toml:term_2_binary(Response)
            end
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request([UserIdBin], <<"UPDATE">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, ContactId} ->
            UserId = erlang:binary_to_integer(UserIdBin),
            {ok, [_AVersion, BVersion]} = contacts:create(UserId, ContactId),
            Response = {<<"response">>, [{<<"status">>, 0},
                                         {<<"version">>, BVersion}]},
            {ok, TomlBin} = toml:term_2_binary(Response)
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request(_, _, Req) ->
    handler_helper:return404(Req).



%% ===================================================================
%% Internal functions
%% ===================================================================