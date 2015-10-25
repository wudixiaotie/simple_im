%% ===================================================================
%% Author xiaotie
%% 2015-10-23
%% contact handler
%% ===================================================================

-module (contact_handler).

-export([init/2, handle_request/4]).



%% ===================================================================
%% API functions
%% ===================================================================

init(Req, Opts) ->
    handler_helper:init(?MODULE, Req, Opts).



%% ===================================================================
%% Request handler
%% ===================================================================

handle_request([<<"find">>], <<"POST">>, true, Req) ->
    {ok, PostVals, _} = cowboy_req:body_qs(Req),
    case handler_helper:verify_token(PostVals) of
        {error, TomlBin} ->
            ok;
        {ok, UserId} ->
            {<<"version">>, ContactVersionBin} = lists:keyfind(<<"version">>, 1, PostVals),
            ContactVersion = erlang:binary_to_integer(ContactVersionBin),
            {ok, ContactList} = contacts:find(UserId, ContactVersion),
            {ok, ContactListToml} = list_to_toml(ContactList),
            Toml = {<<"response">>, [{<<"status">>, 0}]},
            {ok, TomlBin1} = toml:term_2_binary(Toml),
            {ok, TomlBin2} = toml:term_2_binary(ContactListToml),
            TomlBin = <<TomlBin1/binary, "\r\n", TomlBin2/binary>>
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request([], <<"DELETE">>, true, Req) ->
    {ok, PostVals, _} = cowboy_req:body_qs(Req),
    case handler_helper:verify_token(PostVals) of
        {error, TomlBin} ->
            ok;
        {ok, UserId} ->
            {<<"contact_id">>, ContactIdBin} = lists:keyfind(<<"contact_id">>, 1, PostVals),
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
handle_request(_, _, _, Req) ->
    handler_helper:return404(Req).



%% ===================================================================
%% Internal functions
%% ===================================================================

list_to_toml(ContactList) ->
    list_to_toml(ContactList, []).
list_to_toml([{Id, Name, Phone, Avatar}|T], Result) ->
    Contact = {<<"user">>, [{<<"id">>, Id},
                            {<<"name">>, Name},
                            {<<"phone">>, Phone},
                            {<<"avatar">>, Avatar}]},
    list_to_toml(T, [Contact|Result]);
list_to_toml([], Result) ->
    {ok, Result}.