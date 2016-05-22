%% ===================================================================
%% Author xiaotie
%% 2015-10-26
%% pre_contacts
%% ===================================================================

-module(pre_contacts).

-export([create/3, find/2]).



%% ===================================================================
%% APIs
%% ===================================================================

create(AUserId, BUserId, Message)
    when is_integer(AUserId), is_integer(BUserId) ->
    SQL = <<"SELECT create_pre_contact($1, $2, $3);">>,
    {ok, _, [{Result}]} = postgresql:exec(SQL, [AUserId, BUserId, Message]),

    case Result of
        % succeed
        0 ->
            BUserIdBin = erlang:integer_to_binary(BUserId),
            SSDBName = <<"pre_contacts_", BUserIdBin/binary>>,
            SSDBKey = erlang:term_to_binary({AUserId, Message}),
            SSDBScore = erlang:integer_to_binary(utility:timestamp()),
            [<<"ok">>, _] = ssdb:q([<<"zset">>, SSDBName, SSDBKey, SSDBScore]),

            SSDBName1 = <<"pre_contacts_timestamp_", BUserIdBin/binary>>,
            SSDBKey1 = erlang:integer_to_binary(AUserId),
            [<<"ok">>, _] = ssdb:q([<<"hset">>, SSDBName1, SSDBKey1, SSDBScore]);
        _ ->
            ok
    end,
    {ok, Result}.


find(UserId, Timestamp) ->
    UserIdBin = erlang:integer_to_binary(UserId),
    TimestampBin = erlang:integer_to_binary(Timestamp),
    case ssdb:q([<<"zrscan">>, <<"pre_contacts_", UserIdBin/binary>>, <<>>, <<>>, TimestampBin, <<"-1">>]) of
        [<<"ok">>|SSDBResult] ->
            {ok, PreContactsList} = unpack_ssdb(SSDBResult),
            {ok, Result} = to_toml(PreContactsList),
            {ok, Result};
        _ ->
            log:e("[SSDB] pre_contacts: find id error id:~p!~n", [UserId]),
            SQL = <<"SELECT a_id, message, extract(epoch from updated_at)::integer ",
                    "FROM pre_contacts ",
                    "WHERE b_id = $1 ",
                    "AND extract(epoch from updated_at)::integer > $2 ",
                    "ORDER BY updated_at DESC;">>,
            {ok, _, PreContactsList} = postgresql:exec(SQL, [UserId, Timestamp]),
            {ok, Result} = to_toml(PreContactsList),
            {ok, Result}
    end.



%% ===================================================================
%% Internal functions
%% ===================================================================

unpack_ssdb(SSDBResult) ->
    unpack_ssdb(SSDBResult, []).
unpack_ssdb([], Result) ->
    {ok, Result};
unpack_ssdb([Key, Score|T], Result) ->
    {UserId, Message} = erlang:binary_to_term(Key),
    Timestamp = erlang:binary_to_integer(Score),
    Item = {UserId, Message, Timestamp},
    unpack_ssdb(T, [Item|Result]).


to_toml(PreContactsList) ->
    to_toml(PreContactsList, []).
to_toml([{UserId, Message, Timestamp}|T], Result) ->
    Item = {<<"pre_contact">>, [{<<"user_id">>, UserId},
                                {<<"message">>, Message},
                                {<<"timestamp">>, Timestamp}]},
    to_toml(T, [Item|Result]);
to_toml([], Result) ->
    NewResult = lists:reverse(Result),
    {ok, NewResult}.