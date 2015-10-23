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

handle_request([<<"get">>], <<"POST">>, true, Req) ->
    {ok, PostVals, _} = cowboy_req:body_qs(Req),
    case handler_helper:verify_token(PostVals) of
        {error, TomlBin} ->
            ok;
        {ok, UserId} ->
            {<<"version">>, ContactVersion} = lists:keyfind(<<"version">>, 1, PostVals),
            {ok, ContactIdList} = contacts:find(UserId, ContactVersion),
            Toml = {<<"response">>, [{<<"status">>, 0}]},
            {ok, TomlBin1} = toml:term_2_binary(Toml),
            TomlBin = erlang:list_to_binary(ContactIdList)
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request(_, _, _, Req) ->
    handler_helper:return404(Req).



%% ===================================================================
%% Internal functions
%% ===================================================================
