%% ===================================================================
%% Author xiaotie
%% 2016-05-17
%% pre_contact handler
%% ===================================================================

-module(pre_contact_handler).

-export([init/2, handle_request/3]).



%% ===================================================================
%% API functions
%% ===================================================================

init(Req, Opts) ->
    handler_helper:init(?MODULE, Req, Opts).



%% ===================================================================
%% Request handler
%% ===================================================================

handle_request([TimestampBin], <<"GET">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, UserId, _} ->
            Timestamp = erlang:binary_to_integer(TimestampBin),
            {ok, Toml2} = pre_contacts:find(UserId, Timestamp),
            Toml1 = {<<"response">>, [{<<"status">>, 0}]},
            {ok, TomlBin1} = toml:term_2_binary(Toml1),
            {ok, TomlBin2} = toml:term_2_binary(Toml2),
            TomlBin = <<TomlBin1/binary, "\r\n", TomlBin2/binary>>
    end,
    handler_helper:return(200, TomlBin, Req);
handle_request(_, _, Req) ->
    handler_helper:return404(Req).



%% ===================================================================
%% Internal functions
%% ===================================================================