%% ===================================================================
%% Author xiaotie
%% 2015-09-14
%% users data logic module
%% ===================================================================

-module(users).

-export([create/3, verify/2, find/1, find_groups/2, update/2, to_toml/1]).



%% ===================================================================
%% APIs
%% ===================================================================

create(Name, Phone, Password) ->
    {ok, Salt} = utility:random_binary_16(),
    {ok, EncryptedPassword} = utility:md5_hex_32(<<Password/binary, Salt/binary>>),
    SQL = <<"SELECT create_user($1, $2, $3, $4);">>,
    {ok, _, [{Result}]} = postgresql:exec(SQL, [Name,
                                                Phone,
                                                EncryptedPassword,
                                                Salt]),
    case Result of
        0 ->
            SQL1 = <<"SELECT id FROM users where phone = $1;">>,
            {ok, _, [{UserId}]} = postgresql:exec(SQL1, [Phone]),
            UserIdBin = erlang:integer_to_binary(UserId),
            [<<"ok">>, _] = ssdb:q([<<"multi_hset">>, <<"users_", UserIdBin/binary>>,
                                    <<"name">>, Name,
                                    <<"phone">>, Phone,
                                    <<"password">>, EncryptedPassword,
                                    <<"salt">>, Salt,
                                    <<"avatar">>, <<>>]),
            [<<"ok">>, _] = ssdb:q([<<"multi_hset">>, <<"users_phone_", Phone/binary>>,
                                    <<"name">>, Name,
                                    <<"id">>, UserIdBin,
                                    <<"password">>, EncryptedPassword,
                                    <<"salt">>, Salt,
                                    <<"avatar">>, <<>>]),
            ok;
        1 ->
            {error, user_exist};
        _ ->
            {error, unknown}
    end.


verify(Phone, Password) ->
    case ssdb:q([<<"multi_hget">>, <<"users_phone_", Phone/binary>>, <<"id">>, <<"password">>, <<"salt">>]) of
        [<<"ok">>, <<"id">>, UserIdBin, <<"password">>, EncryptedPassword, <<"salt">>, Salt] ->
            UserId = erlang:binary_to_integer(UserIdBin),
            verify(UserId, Password, EncryptedPassword, Salt);
        [<<"ok">>] ->
            {error, user_does_not_exist};
        _ ->
            log:e("[SSDB] users: verify error phone:~p, password:~p!~n", [Phone, Password]),
            SQL = <<"SELECT id, password, salt FROM users WHERE phone = $1;">>,
            case postgresql:exec(SQL, [Phone]) of
                {ok, _, []} ->
                    {error, user_does_not_exist};
                {ok, _, [{UserId, EncryptedPassword, Salt}]} ->
                    verify(UserId, Password, EncryptedPassword, Salt);
                {error, Reason} ->
                    {error, Reason}
            end
    end.


find({id, UserId}) ->
    UserIdBin = erlang:integer_to_binary(UserId),
    case catch ssdb:q([<<"multi_hget">>, <<"users_", UserIdBin/binary>>, <<"name">>, <<"phone">>, <<"avatar">>]) of
        [<<"ok">>, <<"name">>, Name, <<"phone">>, Phone, <<"avatar">>, Avatar] ->
            {ok, [{UserId, Name, Phone, Avatar}]};
        _ ->
            log:e("[SSDB] users: find id error id:~p!~n", [UserId]),
            SQL = <<"SELECT id, name, phone, avatar FROM users WHERE id = $1;">>,
            {ok, _, Result} = postgresql:exec(SQL, [UserId]),
            {ok, Result}
    end;
find({phone, Phone}) ->
    case catch ssdb:q([<<"multi_hget">>, <<"users_phone_", Phone/binary>>, <<"id">>, <<"name">>, <<"avatar">>]) of
        [<<"ok">>, <<"id">>, UserIdBin, <<"name">>, Name, <<"avatar">>, Avatar] ->
            UserId = erlang:binary_to_integer(UserIdBin),
            {ok, [{UserId, Name, Phone, Avatar}]};
        _ ->
            log:e("[SSDB] users: find phone error phone:~p!~n", [Phone]),
            SQL = <<"SELECT id, name, phone, avatar FROM users WHERE phone = $1;">>,
            {ok, _, Result} = postgresql:exec(SQL, [Phone]),
            {ok, Result}
    end.


find_groups(UserId, Timestamp) ->
    UserIdBin = erlang:integer_to_binary(UserId),
    TimestampBin = erlang:integer_to_binary(Timestamp),
    case catch ssdb:q([<<"zscan">>, <<"user_groups_", UserIdBin/binary>>, <<>>, TimestampBin, <<>>, <<"-1">>]) of
        [<<"ok">>|GroupIdList] ->
            {ok, Toml} = find_group_info_ssdb(GroupIdList),
            {ok, Toml};
        _ ->
            log:e("[SSDB] user: find_groups error id:~p timestamp:~p!~n", [UserId, Timestamp]),
            SQL = <<"SELECT g.id,
                            g.name,
                            g.creator_id,
                            g.key,
                            extract(epoch from g.created_at):: integer
                    FROM groups g, group_members gm
                    WHERE g.id = gm.group_id
                    AND gm.user_id = $1
                    AND gm.created_at > $2;">>,
            {ok, _, GroupInfoList} = postgresql:exec(SQL, [UserId, Timestamp]),
            {ok, Toml} = find_group_info_odbc(GroupInfoList),
            {ok, Toml}
    end.


update(UserId, {name, NewNameBin}) ->
    SQL = <<"UPDATE users SET name = $2 WHERE id = $1;">>,
    case postgresql:exec(SQL, [UserId, NewNameBin]) of
        {ok, 1} ->
            UserIdBin = erlang:integer_to_binary(UserId),
            [<<"ok">>, PhoneBin] = ssdb:q([<<"hget">>, <<"users_", UserIdBin/binary>>, <<"phone">>]),

            [<<"ok">>, _] = ssdb:q([<<"hset">>, <<"users_", UserIdBin/binary>>, <<"name">>, NewNameBin]),
            [<<"ok">>, _] = ssdb:q([<<"hset">>, <<"users_phone_", PhoneBin/binary>>, <<"name">>, NewNameBin]),
            ok;
        _ ->
            error
    end;
update(UserId, {phone, NewPhoneBin}) ->
    SQL = <<"UPDATE users SET phone = $2 WHERE id = $1;">>,
    case postgresql:exec(SQL, [UserId, NewPhoneBin]) of
        {ok, 1} ->
            UserIdBin = erlang:integer_to_binary(UserId),
            [<<"ok">>, PhoneBin] = ssdb:q([<<"hget">>, <<"users_", UserIdBin/binary>>, <<"phone">>]),
            [<<"ok">>|Result] = ssdb:q([<<"hgetall">>, <<"users_phone_", PhoneBin/binary>>]),

            [<<"ok">>, _] = ssdb:q([<<"hset">>, <<"users_", UserIdBin/binary>>, <<"phone">>, NewPhoneBin]),
            [<<"ok">>, _] = ssdb:q([<<"multi_hset">>, <<"users_phone_", NewPhoneBin/binary>>|Result]),
            [<<"ok">>, _] = ssdb:q([<<"hclear">>, <<"users_phone_", PhoneBin/binary>>]),
            ok;
        _ ->
            error
    end;
update(UserId, {avatar, NewAvatarBin}) ->
    SQL = <<"UPDATE users SET avatar = $2 WHERE id = $1;">>,
    case postgresql:exec(SQL, [UserId, NewAvatarBin]) of
        {ok, 1} ->
            UserIdBin = erlang:integer_to_binary(UserId),
            [<<"ok">>, PhoneBin] = ssdb:q([<<"hget">>, <<"users_", UserIdBin/binary>>, <<"phone">>]),

            [<<"ok">>, _] = ssdb:q([<<"hset">>, <<"users_", UserIdBin/binary>>, <<"avatar">>, NewAvatarBin]),
            [<<"ok">>, _] = ssdb:q([<<"hset">>, <<"users_phone_", PhoneBin/binary>>, <<"avatar">>, NewAvatarBin]),
            ok;
        _ ->
            error
    end.


to_toml(TupleList) ->
    to_toml(TupleList, []).


%% ===================================================================
%% Internal functions
%% ===================================================================

verify(UserId, Password, EncryptedPassword, Salt) ->
    case utility:md5_hex_32(<<Password/binary, Salt/binary>>) of
        {ok, EncryptedPassword} ->
            {ok, true, UserId};
        _ ->
            {ok, false}
    end.    


find_group_info_ssdb(GroupIdList) ->
    find_group_info_ssdb(GroupIdList, []).
find_group_info_ssdb([GroupIdBin, _|T], Toml) ->
    GroupId = erlang:binary_to_integer(GroupIdBin),
    case ssdb:q([<<"multi_hget">>, <<"groups_", GroupIdBin/binary>>, <<"name">>, <<"creator_id">>, <<"key">>, <<"created_at">>]) of
        [<<"ok">>, <<"name">>, Name, <<"creator_id">>, CreatorIdBin, <<"key">>, Key, <<"created_at">>, CreatedAtBin] ->
            GroupToml = {<<"group">>, [{<<"id">>, GroupId},
                                       {<<"name">>, Name},
                                       {<<"creator_id">>, erlang:binary_to_integer(CreatorIdBin)},
                                       {<<"key">>, Key},
                                       {<<"created_at">>, erlang:binary_to_integer(CreatedAtBin)}]},
            find_group_info_ssdb(T, [GroupToml|Toml]);
        _ ->
            find_group_info_ssdb(T, Toml)
    end;
find_group_info_ssdb([], Toml) ->
    {ok, Toml}.


find_group_info_odbc(GroupInfoList) ->
    find_group_info_odbc(GroupInfoList, []).
find_group_info_odbc([{GroupId, Name, CreatorId, Key, CreatedAt}|T], Toml) ->
    GroupToml = {<<"group">>, [{<<"id">>, GroupId},
                               {<<"name">>, Name},
                               {<<"creator_id">>, CreatorId},
                               {<<"key">>, Key},
                               {<<"created_at">>, CreatedAt}]},
    find_group_info_odbc(T, [GroupToml|Toml]);
find_group_info_odbc([], Toml) ->
    {ok, Toml}.


to_toml([{UserId, Name, Phone, Avatar}|T], Toml) ->
    UserToml = {<<"user">>, [{<<"id">>, UserId},
                             {<<"name">>, Name},
                             {<<"phone">>, Phone},
                             {<<"avatar">>, Avatar}]},
    to_toml(T, [UserToml|Toml]);
to_toml([], Toml) ->
    {ok, Toml}.