%% ===================================================================
%% Author xiaotie
%% 2015-9-2
%% offline
%% ===================================================================

-module (offline).

-export ([store/2, get/1]).

% offline message expired after 7 days
-define (OFFLINE_EXPIRATION_TIME, 604800).



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

store(UserId, [{MsgId, MsgTuple}|T], MsgIdList) when is_tuple(MsgTuple) ->
    {ok, MsgBin} = utility:tuple_to_toml(MsgTuple),
    store(UserId, [{MsgId, MsgBin}|T], MsgIdList);
store(UserId, [{MsgId, MsgBin}|T], MsgIdList) ->
    {ok, <<"OK">>} = redis:q([<<"SETEX">>, MsgId, ?OFFLINE_EXPIRATION_TIME, MsgBin]),
    store(UserId, T, [MsgId|MsgIdList]);
store(UserId, [], MsgIdList) ->
    {ok, Key} = make_key(UserId),
    {ok, _} = redis:q([<<"RPUSH">>, Key] ++ MsgIdList),
    ok.


make_key(UserId) ->
    UserIdBin = integer_to_binary(UserId),
    Key = <<"offline_", UserIdBin/binary>>,
    {ok, Key}.