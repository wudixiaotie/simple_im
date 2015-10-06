%% ===================================================================
%% Author xiaotie
%% 2015-10-5
%% offline handler
%% ===================================================================

-module (offline_handler).

-export([init/2]).



%% ===================================================================
%% API functions
%% ===================================================================

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req),
    Req2 = handle_request(Method, HasBody, Req),
    {ok, Req2, Opts}.



%% ===================================================================
%% Internal functions
%% ===================================================================

handle_request(<<"POST">>, true, Req) ->
    {ok, PostVals, _} = cowboy_req:body_qs(Req),
    {<<"user_id">>, UserIdBin} = lists:keyfind(<<"user_id">>, 1, PostVals),
    UserId = erlang:binary_to_integer(UserIdBin),
    {ok, MsgList} = offline:get(UserId),
    {ok, Result} = list_2_binary(MsgList),
    cowboy_req:reply(200, [], Result, Req).


list_2_binary(MsgList) ->
    list_2_binary(MsgList, <<>>).
list_2_binary([H|T], Result) ->
    list_2_binary(T, <<H/binary, "\r\n", Result/binary>>);
list_2_binary([], Result) ->
    {ok, Result}.