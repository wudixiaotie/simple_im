-module(ssdb_recover).

-export([start/0]).
-compile (export_all).


start() ->
    ssdb:start_link(),
    case postgresql:exec(<<"SELECT id, name, phone, avatar, password, salt FROM users;">>) of
        {ok, _, UserList} ->
            ok = store_ssdb_user(UserList),
            ok;
        _ ->
            ok
    end,
    case postgresql:exec(<<"SELECT id, name, creator_id, key, extract(epoch from created_at)::integer FROM groups;">>) of
        {ok, _, GroupList} ->
            ok = store_ssdb_group(GroupList),
            ok;
        _ ->
            ok
    end,
    case postgresql:exec(<<"SELECT user_id, contact_id, contact_version FROM contacts;">>) of
        {ok, _, ContactList} ->
            ok = store_ssdb_contact(ContactList),
            ok;
        _ ->
            ok
    end.


store_ssdb_user([{UserId, Name, Phone, Avatar, EncryptedPassword, Salt}|T]) ->
    UserIdBin = erlang:integer_to_binary(UserId),
    [<<"ok">>, _] = ssdb:q([<<"multi_hset">>, <<"users_", UserIdBin/binary>>,
                            <<"name">>, Name,
                            <<"phone">>, Phone,
                            <<"password">>, EncryptedPassword,
                            <<"salt">>, Salt,
                            <<"avatar">>, Avatar]),
    [<<"ok">>, _] = ssdb:q([<<"multi_hset">>, <<"users_phone_", Phone/binary>>,
                            <<"name">>, Name,
                            <<"id">>, UserIdBin,
                            <<"password">>, EncryptedPassword,
                            <<"salt">>, Salt,
                            <<"avatar">>, Avatar]),
    store_ssdb_user(T);
store_ssdb_user([]) ->
    ok.


store_ssdb_group([{GroupId, Name, CreatorId, Key, CreatedAt}|T]) ->
    GroupIdBin = erlang:integer_to_binary(GroupId),
    CreatorIdBin = erlang:integer_to_binary(CreatorId),
    CreatedAtBin = erlang:integer_to_binary(CreatedAt),
    [<<"ok">>, _] = ssdb:q([<<"multi_hset">>, <<"groups_", GroupIdBin/binary>>,
                            <<"name">>, Name,
                            <<"creator_id">>, CreatorIdBin,
                            <<"key">>, Key,
                            <<"created_at">>, CreatedAtBin]),
    case postgresql:exec(<<"SELECT group_id, user_id, extract(epoch from created_at)::integer FROM group_members where group_id = $1;">>, [GroupId]) of
        {ok, _, GroupMemberList} ->
            ok = store_ssdb_group_member(GroupMemberList),
            ok;
        _ ->
            ok
    end,
    store_ssdb_group(T);
store_ssdb_group([]) ->
    ok.


store_ssdb_group_member([{GroupId, UserId, CreatedAt}|T]) ->
    GroupIdBin = erlang:integer_to_binary(GroupId),
    UserIdBin = erlang:integer_to_binary(UserId),
    CreatedAtBin = erlang:integer_to_binary(CreatedAt),
    [<<"ok">>, _] = ssdb:q([<<"zset">>, <<"user_groups_", UserIdBin/binary>>,
                            GroupIdBin, CreatedAtBin]),
    [<<"ok">>, _] = ssdb:q([<<"zset">>, <<"group_members_", GroupIdBin/binary>>,
                            UserIdBin, CreatedAtBin]),
    store_ssdb_group_member(T);
store_ssdb_group_member([]) ->
    ok.


store_ssdb_contact([{UserId, ContactId, ContactVersion}|T]) ->
    UserIdBin = erlang:integer_to_binary(UserId),
    ContactIdBin = erlang:integer_to_binary(ContactId),
    ContactVersionBin = erlang:integer_to_binary(ContactVersion),
    [<<"ok">>, _] = ssdb:q([<<"zset">>, <<"contacts_", UserIdBin/binary>>, ContactIdBin, ContactVersionBin]),
    store_ssdb_contact(T);
store_ssdb_contact([]) ->
    ok.