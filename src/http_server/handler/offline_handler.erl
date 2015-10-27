%% ===================================================================
%% Author xiaotie
%% 2015-10-5
%% offline handler
%% ===================================================================

-module (offline_handler).

-export([init/2, handle_request/3]).



%% ===================================================================
%% API functions
%% ===================================================================

init(Req, Opts) ->
    handler_helper:init(?MODULE, Req, Opts).



%% ===================================================================
%% Request handler
%% ===================================================================

handle_request([], <<"GET">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, UserId} ->
            {ok, MsgList} = offline:get(UserId),
            Toml = {<<"response">>, [{<<"status">>, 0}]},
            {ok, TomlBin1} = toml:term_2_binary(Toml),
            {ok, TomlBin2} = list_2_binary(MsgList),
            TomlBin = <<TomlBin1/binary, "\r\n", TomlBin2/binary>>
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request([], <<"DELETE">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, UserId} ->
            ok = offline:clean(UserId),
            Toml = {<<"response">>, [{<<"status">>, 0}]},
            {ok, TomlBin} = toml:term_2_binary(Toml)
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request(_, _, Req) ->
    handler_helper:return404(Req).



%% ===================================================================
%% Internal functions
%% ===================================================================

list_2_binary(MsgList) ->
    list_2_binary(MsgList, <<>>).
list_2_binary([H|T], Result) ->
    list_2_binary(T, <<H/binary, "\r\n", Result/binary>>);
list_2_binary([], Result) ->
    {ok, Result}.