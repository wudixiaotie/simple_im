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

            SQL1 = <<"SELECT c.user_id, c.contact_id, u.name, u.phone, u.avatar, c.contact_version
                      FROM users u, contacts c
                      WHERE u.id = c.contact_id
                      AND ((c.user_id = $1 AND c.contact_id = $2) OR
                           (c.user_id = $2 AND c.contact_id = $1));">>,
            {ok, _, ContactList} = postgresql:exec(SQL1, [AUserId, BUserId]),
            ok = create_ssdb(ContactList);
        _ ->
          ok  
    end,
    {ok, Result}.


delete(AUserId, BUserId)
    when is_integer(AUserId), is_integer(BUserId) ->
    SQL1 = <<"SELECT user_id, contact_version
              FROM contacts
              WHERE (user_id = $1 AND contact_id = $2)
              OR (user_id = $2 AND contact_id = $1);">>,
    {ok, _, VersionList} = postgresql:exec(SQL1, [AUserId, BUserId]),

    SQL = <<"SELECT delete_contact($1, $2);">>,
    {ok, _, [{Result}]} = postgresql:exec(SQL, [AUserId, BUserId]),
    case Result of
        0 ->
            ok = delete_ssdb(VersionList),
            ok;
        _ ->
            ok
    end.


find(UserId, 0) when is_integer(UserId) ->
    UserIdBin = erlang:integer_to_binary(UserId),
    case ssdb:q([<<"zscan">>, <<"contacts_", UserIdBin/binary>>, <<>>, <<>>, <<>>, <<"-1">>]) of
        [<<"ok">>|SSDBResult] ->
            {ok, ContactsList} = unpack_ssdb(SSDBResult),
            % hack how to get current max version
            {ok, 0, ContactsList};
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

            SQLVersion = <<"SELECT contact_version FROM users WHERE id = $1">>,
            {ok, _, [{CurrentVersion}]} = postgresql:exec(SQLVersion, [UserId]),
            {ok, CurrentVersion, Result}
    end;
find(UserId, ContactVersion)
    when is_integer(UserId), is_integer(ContactVersion) ->
    UserIdBin = erlang:integer_to_binary(UserId),
    ContactVersionBin = erlang:integer_to_binary(ContactVersion),
    case ssdb:q([<<"zscan">>, <<"contacts_", UserIdBin/binary>>, <<>>, ContactVersionBin, <<>>, <<"-1">>]) of
        [<<"ok">>|SSDBResult] ->
            {ok, ContactsList} = unpack_ssdb(SSDBResult),
            % hack how to get current max version
            {ok, 0, ContactsList};
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

            SQLVersion = <<"SELECT contact_version FROM users WHERE id = $1">>,
            {ok, _, [{CurrentVersion}]} = postgresql:exec(SQLVersion, [UserId]),
            {ok, CurrentVersion, Result}
    end.



%% ===================================================================
%% Internal functions
%% ===================================================================

create_ssdb([{UserId, ContactId, ContactName, ContactPhone, ContactAvatar, ContactVersion}|T]) ->
    UserIdBin = erlang:integer_to_binary(UserId),
    Name = <<"contacts_", UserIdBin/binary>>,
    Key = erlang:term_to_binary({ContactId, ContactName, ContactPhone, ContactAvatar}),
    Score = erlang:integer_to_binary(ContactVersion),
    [<<"ok">>, _] = ssdb:q([<<"zset">>, Name, Key, Score]),
    create_ssdb(T);
create_ssdb([]) ->
    ok.


delete_ssdb([{UserId, ContactVersion}|T]) ->
    UserIdBin = erlang:integer_to_binary(UserId),
    Name = <<"contacts_", UserIdBin/binary>>,
    Score = erlang:integer_to_binary(ContactVersion),
    [<<"ok">>, _] = ssdb:q([<<"zremrangebyscore">>, Name, Score, Score]),
    delete_ssdb(T);
delete_ssdb([]) ->
    ok.


unpack_ssdb(SSDBResult) ->
    unpack_ssdb(SSDBResult, []).
unpack_ssdb([], Result) ->
    {ok, Result};
unpack_ssdb([Key, _|T], Result) ->
    {UserId, Name, Phone, Avatar} = erlang:binary_to_term(Key),
    unpack_ssdb(T, [{UserId, Name, Phone, Avatar}|Result]).