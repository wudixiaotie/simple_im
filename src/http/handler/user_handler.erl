%% ===================================================================
%% Author xiaotie
%% 2015-10-26
%% contact handler
%% ===================================================================

-module(user_handler).

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
        {ok, _, _} ->
            case users:find({phone, PhoneBin}) of
                {ok, []} ->
                    {ok, TomlBin} = handler_helper:success();
                {ok, Result} ->
                    {ok, TomlBin1} = handler_helper:success(),
                    {ok, Toml2} = users:to_toml(Result),
                    {ok, TomlBin2} = toml:term_2_binary(Toml2),
                    TomlBin = <<TomlBin1/binary, "\r\n", TomlBin2/binary>>
            end
    end,
    handler_helper:return(200, TomlBin, Req);
handle_request([<<"id">>, IdBin], <<"GET">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, _, _} ->
            Id = erlang:binary_to_integer(IdBin),
            case users:find({id, Id}) of
                {ok, []} ->
                    {ok, TomlBin} = handler_helper:success();
                {ok, Result} ->
                    {ok, TomlBin1} = handler_helper:success(),
                    {ok, Toml2} = users:to_toml(Result),
                    {ok, TomlBin2} = toml:term_2_binary(Toml2),
                    TomlBin = <<TomlBin1/binary, "\r\n", TomlBin2/binary>>
            end
    end,
    handler_helper:return(200, TomlBin, Req);
handle_request([], <<"POST">>, Req) ->
    {ok, PostVals, _} = cowboy_req:body_qs(Req),
    case utility:check_parameters([<<"name">>, <<"phone">>, <<"password">>], PostVals) of
        {ok, [Name, Phone, Password]} ->
            case users:create(Name, Phone, Password) of
                ok ->
                    {ok, TomlBin} = handler_helper:success();
                {error, user_exist} ->
                    {ok, TomlBin} = handler_helper:error(1, <<"User Exist">>);
                _ ->
                    {ok, TomlBin} = handler_helper:error(2, <<"Unknown reason">>)
            end;
        {error, Reason} ->
            {ok, TomlBin} = handler_helper:error(3, Reason)
    end,
    handler_helper:return(200, TomlBin, Req);
handle_request(_, _, Req) ->
    handler_helper:return404(Req).



%% ===================================================================
%% Internal functions
%% ===================================================================