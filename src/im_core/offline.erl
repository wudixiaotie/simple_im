%% ===================================================================
%% Author xiaotie
%% 2015-9-2
%% offline
%% ===================================================================

-module (offline).

-export ([store/2, get/1]).

% offline message expired after 7 days
-define (OFFLINE_EXPIRATION_TIME, 604800).

-include("message.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

store(UserId, MsgList) ->
    store(UserId, MsgList, []).


get(UserId) ->
    {ok, Key} = make_key(UserId),
    case redis:q([<<"LRANGE">>, Key, <<"0">>, <<"-1">>]) of
        {ok, []} ->
            {ok, []};
        {ok, MsgIdList} ->
            {ok, Result} = redis:q([<<"MGET">>|MsgIdList]),
            {ok, _} = redis:q([<<"DEL">>, Key] ++ MsgIdList),
            utility:delete_from_list(undefined, Result)
    end.



%% ===================================================================
%% Internal functions
%% ===================================================================

store(UserId, [Message|T], MsgIdList) ->
    {ok, MsgBin} = toml:term_2_binary(Message#message.toml),
    MsgId = Message#message.id,
    {ok, <<"OK">>} = redis:q([<<"SETEX">>, MsgId, ?OFFLINE_EXPIRATION_TIME, MsgBin]),
    store(UserId, T, [MsgId|MsgIdList]);
store(_, [], []) ->
    ok;
store(UserId, [], MsgIdList) ->
    {ok, Key} = make_key(UserId),
    {ok, _} = redis:q([<<"RPUSH">>, Key] ++ MsgIdList),
    ok.


make_key(UserId) ->
    UserIdBin = integer_to_binary(UserId),
    Key = <<"offline_", UserIdBin/binary>>,
    {ok, Key}.