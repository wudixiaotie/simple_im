%% ===================================================================
%% Author xiaotie
%% 2015-9-2
%% offline
%% ===================================================================

-module(offline).

-export([store/2, get/1, clean/1]).

% offline message expired after 7 days
-define(OFFLINE_EXPIRATION_TIME, 604800).

-include("message.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

store(UserId, MsgList) ->
    store(UserId, MsgList, []).


get(UserId) ->
    {ok, Key} = redis:key({offline, UserId}),
    case redis:q([<<"LRANGE">>, Key, <<"0">>, <<"-1">>]) of
        {ok, []} ->
            {ok, []};
        {ok, MsgIdList} ->
            {ok, Result} = redis:q([<<"MGET">>|MsgIdList]),
            utility:delete_from_list(undefined, Result)
    end.


clean(UserId) ->
    {ok, Key} = redis:key({offline, UserId}),
    case redis:q([<<"LRANGE">>, Key, <<"0">>, <<"-1">>]) of
        {ok, []} ->
            ok;
        {ok, MsgIdList} ->
            {ok, _} = redis:q([<<"DEL">>, Key] ++ MsgIdList),
            ok
    end.



%% ===================================================================
%% Internal functions
%% ===================================================================

store(UserId, [Message|T], OfflineMsgKeyList) ->
    {ok, OfflineMsgKey} = redis:key({offline_msg, UserId, Message#message.id}),
    {ok, <<"OK">>} = redis:q([<<"SETEX">>, OfflineMsgKey, ?OFFLINE_EXPIRATION_TIME, Message#message.bin]),
    store(UserId, T, [OfflineMsgKey|OfflineMsgKeyList]);
store(_, [], []) ->
    ok;
store(UserId, [], OfflineMsgKeyList) ->
    {ok, Key} = redis:key({offline, UserId}),
    {ok, _} = redis:q([<<"RPUSH">>, Key] ++ OfflineMsgKeyList),
    ok.