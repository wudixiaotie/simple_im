%% ===================================================================
%% Author xiaotie
%% 2015-09-14
%% users data logic module
%% ===================================================================

-module(contacts).

-export([create/2, find/2, delete/2]).



%% ===================================================================
%% APIs
%% ===================================================================

create(AUserId, BUserId)
    when is_integer(AUserId), is_integer(BUserId) ->
    SQL = <<"SELECT create_contact($1, $2);">>,
    {ok, _, [{Result}]} = postgresql:exec(SQL, [AUserId, BUserId]),
    case Result of
        0 ->
            % delete pre_contact from ssdb
            AUserIdBin = erlang:integer_to_binary(AUserId),
            BUserIdBin = erlang:integer_to_binary(BUserId),
            Key = <<"pre_contacts_timestamp_", BUserIdBin/binary>>,
            [<<"ok">>, TimestampBin] = ssdb:q([<<"hget">>, Key, AUserIdBin]),
            [<<"ok">>, _] = ssdb:q([<<"hdel">>, Key, AUserIdBin]),
            [<<"ok">>, _] = ssdb:q([<<"zremrangebyscore">>,
                                    <<"pre_contacts_", BUserIdBin/binary>>,
                                    TimestampBin,
                                    TimestampBin]),

            SQL1 = <<"SELECT user_id, contact_id, contact_version
                      FROM contacts
                      WHERE (user_id = $1 AND contact_id = $2)
                      OR (user_id = $2 AND contact_id = $1);">>,
            {ok, _, Result1} = postgresql:exec(SQL1, [AUserId, BUserId]),
            ok = create_ssdb(Result1);
        _ ->
          ok  
    end,
    {ok, Result}.


delete(AUserId, BUserId)
    when is_integer(AUserId), is_integer(BUserId) ->
    SQL = <<"SELECT delete_contact($1, $2);">>,
    {ok, _, [{Result}]} = postgresql:exec(SQL, [AUserId, BUserId]),
    case Result of
        0 ->
            AUserIdBin = erlang:integer_to_binary(AUserId),
            BUserIdBin = erlang:integer_to_binary(BUserId),
            [<<"ok">>, _] = ssdb:q([<<"zdel">>, <<"contacts_", AUserIdBin/binary>>, BUserIdBin]),
            [<<"ok">>, _] = ssdb:q([<<"zdel">>, <<"contacts_", BUserIdBin/binary>>, AUserIdBin]),
            ok;
        _ ->
            ok
    end.


find(UserId, 0) when is_integer(UserId) ->
    UserIdBin = erlang:integer_to_binary(UserId),
    case ssdb:q([<<"zscan">>, <<"contacts_", UserIdBin/binary>>, <<>>, <<>>, <<>>, <<"-1">>]) of
        [<<"ok">>|ContactIdList] ->
            {ok, ContactsList} = find_contact_info(ContactIdList),
            % hack how to get current max version
            {ok, ContactsList};
        _ ->
            log:e("[SSDB] contacts: find error id:~p version:0!~n", [UserId]),
            SQL = <<"SELECT u.id,
                            u.name,
                            u.phone,
                            u.avatar
                     FROM users u, contacts c
                     WHERE u.id = c.contact_id
                     AND c.user_id = $1;">>,
            {ok, _, Result} = postgresql:exec(SQL, [UserId]),
            {ok, Result}
    end;
find(UserId, ContactVersion)
    when is_integer(UserId), is_integer(ContactVersion) ->
    UserIdBin = erlang:integer_to_binary(UserId),
    ContactVersionBin = erlang:integer_to_binary(ContactVersion),
    case ssdb:q([<<"zscan">>, <<"contacts_", UserIdBin/binary>>, <<>>, ContactVersionBin, <<>>, <<"-1">>]) of
        [<<"ok">>|ContactIdList] ->
            {ok, ContactsList} = find_contact_info(ContactIdList),
            {ok, ContactsList};
        _ ->
            log:e("[SSDB] contacts: find error id:~p version:0!~n", [UserId]),
            SQL = <<"SELECT u.id,
                            u.name,
                            u.phone,
                            u.avatar
                     FROM users u, contacts c
                     WHERE u.id = c.contact_id
                     AND c.user_id = $1
                     AND c.contact_version > $2;">>,
            {ok, _, Result} = postgresql:exec(SQL, [UserId, ContactVersion]),
            {ok, Result}
    end.



%% ===================================================================
%% Internal functions
%% ===================================================================

create_ssdb([{UserId, ContactId, ContactVersion}|T]) ->
    UserIdBin = erlang:integer_to_binary(UserId),
    SSDBName = <<"contacts_", UserIdBin/binary>>,
    SSDBKey = erlang:integer_to_binary(ContactId),
    SSDBScore = erlang:integer_to_binary(ContactVersion),
    [<<"ok">>, _] = ssdb:q([<<"zset">>, SSDBName, SSDBKey, SSDBScore]),
    create_ssdb(T);
create_ssdb([]) ->
    ok.


find_contact_info(ContactIdList) ->
    find_contact_info(ContactIdList, []).
find_contact_info([], Result) ->
    {ok, Result};
find_contact_info([ContactIdBin, _|T], Result) ->
    [<<"ok">>, <<"name">>, Name, <<"phone">>, Phone,
     <<"avatar">>, Avatar] = ssdb:q([<<"multi_hget">>,
                                     <<"users_", ContactIdBin/binary>>,
                                     <<"name">>,
                                     <<"phone">>,
                                     <<"avatar">>]),
    UserId = erlang:binary_to_integer(ContactIdBin),
    find_contact_info(T, [{UserId, Name, Phone, Avatar}|Result]).