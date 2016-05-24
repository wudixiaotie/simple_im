%% ===================================================================
%% Author xiaotie
%% 2015-09-14
%% contacts data logic module
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
    case catch ssdb:q([<<"zscan">>, <<"contacts_", UserIdBin/binary>>, <<>>, <<>>, <<>>, <<"-1">>]) of
        [<<"ok">>|ContactIdList] ->
            {ok, Toml} = unpack_ssdb(ContactIdList),
            {ok, Toml};
        _ ->
            log:e("[SSDB] contacts: find error id:~p version:0!~n", [UserId]),
            SQL = <<"SELECT u.id,
                            u.name,
                            u.phone,
                            u.avatar,
                            c.contact_version
                     FROM users u, contacts c
                     WHERE u.id = c.contact_id
                     AND c.user_id = $1;">>,
            {ok, _, ODBCResult} = postgresql:exec(SQL, [UserId]),
            {ok, Toml} = unpack_odbc(ODBCResult),
            {ok, Toml}
    end;
find(UserId, ContactVersion)
    when is_integer(UserId), is_integer(ContactVersion) ->
    UserIdBin = erlang:integer_to_binary(UserId),
    ContactVersionBin = erlang:integer_to_binary(ContactVersion),
    case catch ssdb:q([<<"zscan">>, <<"contacts_", UserIdBin/binary>>, <<>>, ContactVersionBin, <<>>, <<"-1">>]) of
        [<<"ok">>|ContactIdList] ->
            {ok, Toml} = unpack_ssdb(ContactIdList),
            {ok, Toml};
        _ ->
            log:e("[SSDB] contacts: find error id:~p version:~p!~n", [UserId, ContactVersion]),
            SQL = <<"SELECT u.id,
                            u.name,
                            u.phone,
                            u.avatar,
                            c.contact_version
                     FROM users u, contacts c
                     WHERE u.id = c.contact_id
                     AND c.user_id = $1
                     AND c.contact_version > $2;">>,
            {ok, _, ODBCResult} = postgresql:exec(SQL, [UserId, ContactVersion]),
            {ok, Toml} = unpack_odbc(ODBCResult),
            {ok, Toml}
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


unpack_ssdb(ContactIdList) ->
    unpack_ssdb(ContactIdList, []).
unpack_ssdb([ContactIdBin, ContactVersionBin|T], Toml) ->
    case ssdb:q([<<"multi_hget">>, <<"users_", ContactIdBin/binary>>, <<"name">>, <<"phone">>, <<"avatar">>]) of
        [<<"ok">>, <<"name">>, Name, <<"phone">>, Phone, <<"avatar">>, Avatar] ->
            UserId = erlang:binary_to_integer(ContactIdBin),
            ContactVersion = erlang:binary_to_integer(ContactVersionBin),
            ContactToml = {<<"contact">>, [{<<"id">>, UserId},
                                           {<<"name">>, Name},
                                           {<<"phone">>, Phone},
                                           {<<"avatar">>, Avatar},
                                           {<<"contact_version">>, ContactVersion}]},
            unpack_ssdb(T, [ContactToml|Toml]);
        _ ->
            unpack_ssdb(T, Toml)
    end;
unpack_ssdb([], Toml) ->
    {ok, Toml}.


unpack_odbc(ODBCResult) ->
    unpack_odbc(ODBCResult, []).
unpack_odbc([{UserId, Name, Phone, Avatar, ContactVersion}|T], Toml) ->
    ContactToml = {<<"contact">>, [{<<"id">>, UserId},
                                   {<<"name">>, Name},
                                   {<<"phone">>, Phone},
                                   {<<"avatar">>, Avatar},
                                   {<<"contact_version">>, ContactVersion}]},
    unpack_odbc(T, [ContactToml|Toml]);
unpack_odbc([], Toml) ->
    {ok, Toml}.