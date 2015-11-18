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
handle_request(_, _, Req) ->
    handler_helper:return404(Req).



%% ===================================================================
%% Internal functions
%% ===================================================================