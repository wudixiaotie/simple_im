%% ===================================================================
%% Author xiaotie
%% 2015-10-26
%% contact handler
%% ===================================================================

-module (user_handler).

-export([init/2, handle_request/3]).



%% ===================================================================
%% API functions
%% ===================================================================

init(Req, Opts) ->
    handler_helper:init(?MODULE, Req, Opts).



%% ===================================================================
%% Request handler
%% ===================================================================

handle_request([<<"phone">>, PhoneBin], <<"GET">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, _} ->
            case users:find({phone, PhoneBin}) of
                {ok, []} ->
                    Toml = {<<"response">>, [{<<"status">>, 0}]},
                    {ok, TomlBin} = toml:term_2_binary(Toml);
                {ok, Result} ->
                    Toml1 = {<<"response">>, [{<<"status">>, 0}]},
                    {ok, TomlBin1} = toml:term_2_binary(Toml1),
                    {ok, Toml2} = users:to_toml(Result),
                    {ok, TomlBin2} = toml:term_2_binary(Toml2),
                    TomlBin = <<TomlBin1/binary, "\r\n", TomlBin2/binary>>
            end
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request([<<"id">>, IdBin], <<"GET">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, _} ->
            Id = erlang:binary_to_integer(IdBin),
            case users:find({id, Id}) of
                {ok, []} ->
                    Toml = {<<"response">>, [{<<"status">>, 0}]},
                    {ok, TomlBin} = toml:term_2_binary(Toml);
                {ok, Result} ->
                    Toml1 = {<<"response">>, [{<<"status">>, 0}]},
                    {ok, TomlBin1} = toml:term_2_binary(Toml1),
                    {ok, Toml2} = users:to_toml(Result),
                    {ok, TomlBin2} = toml:term_2_binary(Toml2),
                    TomlBin = <<TomlBin1/binary, "\r\n", TomlBin2/binary>>
            end
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request([], <<"POST">>, Req) ->
    {ok, PostVals, _} = cowboy_req:body_qs(Req),
    {<<"name">>, Name} = lists:keyfind(<<"name">>, 1, PostVals),
    {<<"phone">>, Phone} = lists:keyfind(<<"phone">>, 1, PostVals),
    {<<"password">>, Password} = lists:keyfind(<<"password">>, 1, PostVals),
    case users:create(Name, Phone, Password) of
        ok ->
            Response = {<<"response">>, [{<<"status">>, 0}]},
            {ok, TomlBin} = toml:term_2_binary(Response);
        {error, user_exist} ->
            Response = {<<"response">>, [{<<"status">>, 1},
                                         {<<"reason">>, <<"User Exist">>}]},
            {ok, TomlBin} = toml:term_2_binary(Response);
        _ ->
            Response = {<<"response">>, [{<<"status">>, 2}]},
            {ok, TomlBin} = toml:term_2_binary(Response)
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request(_, _, Req) ->
    handler_helper:return404(Req).



%% ===================================================================
%% Internal functions
%% ===================================================================