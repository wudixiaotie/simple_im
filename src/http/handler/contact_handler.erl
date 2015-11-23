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
            {ok, CurrentVersion, ContactList} = contacts:find(UserId, ContactVersion),
            {ok, Toml2} = users:to_toml(ContactList),
            Toml1 = {<<"response">>, [{<<"status">>, 0},
                                      {<<"version">>, CurrentVersion}]},
            {ok, TomlBin1} = toml:term_2_binary(Toml1),
            {ok, TomlBin2} = toml:term_2_binary(Toml2),
            TomlBin = <<TomlBin1/binary, "\r\n", TomlBin2/binary>>
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request([ToUserIdBin], <<"POST">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, UserId} ->
            {ok, PostVals, _} = cowboy_req:body_qs(Req),
            case utility:check_parameters([<<"ask">>], PostVals) of
                {ok, [Ask]} ->
                    ToUserId = erlang:binary_to_integer(ToUserIdBin),
                    case pre_contacts:create(UserId, ToUserId, Ask) of
                        {ok, 0} ->
                            N = {<<"n">>, [{<<"t">>, <<"add_contact">>},
                                           {<<"from">>, UserId},
                                           {<<"to">>, ToUserId},
                                           {<<"ask">>, Ask},
                                           {<<"ts">>, utility:timestamp()}]},
                            {ok, NBin} = toml:term_2_binary(N),
                            ok = agent:offer_a_reward(NBin),
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
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request([AUserIdBin], <<"PUT">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, BUserId} ->
            AUserId = erlang:binary_to_integer(AUserIdBin),
            case contacts:create(AUserId, BUserId) of
                ok ->
                    N = {<<"n">>, [{<<"t">>, <<"accept_contact">>},
                                   {<<"from">>, BUserId},
                                   {<<"to">>, AUserId},
                                   {<<"ts">>, utility:timestamp()}]},
                    {ok, NBin} = toml:term_2_binary(N),
                    ok = agent:offer_a_reward(NBin),
                    {ok, TomlBin} = handler_helper:success();
                {error, unauthorized} ->
                    {ok, TomlBin} = handler_helper:error(1, <<"Unauthorized operate">>);
                {error, unknown} ->
                    {ok, TomlBin} = handler_helper:error(1, <<"Unkonw Error">>)
            end
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request([ToUserIdBin], <<"DELETE">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, UserId} ->
            ToUserId = erlang:binary_to_integer(ToUserIdBin),
            ok = contacts:delete(UserId, ToUserId),
            N = {<<"n">>, [{<<"t">>, <<"delete_contact">>},
                           {<<"from">>, UserId},
                           {<<"to">>, ToUserId},
                           {<<"ts">>, utility:timestamp()}]},
            {ok, NBin} = toml:term_2_binary(N),
            ok = agent:offer_a_reward(NBin),
            {ok, TomlBin} = handler_helper:success()
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request(_, _, Req) ->
    handler_helper:return404(Req).



%% ===================================================================
%% Internal functions
%% ===================================================================