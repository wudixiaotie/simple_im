%% ===================================================================
%% Author xiaotie
%% 2015-09-14
%% group members data logic module
%% ===================================================================

-module(group_members).

-export([create_by_key/3, create_by_creator/3, find/1, delete/2]).



%% ===================================================================
%% API functions
%% ===================================================================

create_by_key(GroupId, Key, MemberId) ->
    CreatedAt = erlang:system_time(seconds),
    SQL = <<"SELECT create_group_member_by_key($1, $2, $3, $4);">>,
    {ok, _, [{Result}]} = postgresql:exec(SQL, [GroupId, Key, MemberId, CreatedAt]),
    case Result of
        0 ->
            ok = create_ssdb(GroupId, MemberId, CreatedAt),
            ok;
        1 ->
            {error, group_not_exist};
        2 ->
            {error, unauthorized};
        _ ->
            {error, unknown}
    end.


create_by_creator(GroupId, CreatorId, MemberId) ->
    CreatedAt = erlang:system_time(seconds),
    SQL = <<"SELECT create_group_member_by_creator($1, $2, $3, $4);">>,
    {ok, _, [{Result}]} = postgresql:exec(SQL, [GroupId, CreatorId, MemberId, CreatedAt]),
    case Result of
        0 ->
            ok = create_ssdb(GroupId, MemberId, CreatedAt),
            ok;
        1 ->
            {error, group_not_exist};
        2 ->
            {error, unauthorized};
        _ ->
            {error, unknown}
    end.


find(GroupId) ->
    GroupIdBin = erlang:integer_to_binary(GroupId),
    case catch ssdb:q([<<"zscan">>, <<"group_members_", GroupIdBin/binary>>, <<>>, <<>>, <<>>, <<"-1">>]) of
        [<<"ok">>|MemberIdList] ->
            {ok, Toml} = unpack_ssdb(MemberIdList),
            {ok, Toml};
        _ ->
            log:e("[SSDB] group_members: find error id:~p~n", [GroupId]),
            SQL = <<"SELECT u.id,
                            u.name,
                            u.phone,
                            u.avatar
                     FROM users u, group_members gm
                     WHERE u.id = gm.user_id
                     AND gm.group_id = $1;">>,
            {ok, _, ODBCResult} = postgresql:exec(SQL, [GroupId]),
            {ok, Toml} = unpack_odbc(ODBCResult),
            {ok, Toml}
    end.


delete(GroupId, UserId) ->
    SQL = <<"DELETE FROM group_members WHERE group_id = $1 AND user_id = $2;">>,
    {ok, _} = postgresql:exec(SQL, [GroupId, UserId]),

    GroupIdBin = erlang:integer_to_binary(GroupId),
    UserIdBin = erlang:integer_to_binary(UserId),
    [<<"ok">>, _] = ssdb:q([<<"zdel">>, <<"user_groups_", UserIdBin/binary>>, GroupIdBin]),
    [<<"ok">>, _] = ssdb:q([<<"zdel">>, <<"group_members_", GroupIdBin/binary>>, UserIdBin]),
    ok.




%% ===================================================================
%% Internal functions
%% ===================================================================

create_ssdb(GroupId, MemberId, CreatedAt) ->
    GroupIdBin = erlang:integer_to_binary(GroupId),
    MemberIdBin = erlang:integer_to_binary(MemberId),
    CreatedAtBin = erlang:integer_to_binary(CreatedAt),
    [<<"ok">>, _] = ssdb:q([<<"zset">>, <<"user_groups_", MemberIdBin/binary>>,
                            GroupIdBin, CreatedAtBin]),
    [<<"ok">>, _] = ssdb:q([<<"zset">>, <<"group_members_", GroupIdBin/binary>>,
                            MemberIdBin, CreatedAtBin]),
    ok.


unpack_ssdb(MemberIdList) ->
    unpack_ssdb(MemberIdList, []).
unpack_ssdb([UserIdBin, _|T], Toml) ->
    case ssdb:q([<<"multi_hget">>, <<"users_", UserIdBin/binary>>, <<"name">>, <<"phone">>, <<"avatar">>]) of
        [<<"ok">>, <<"name">>, Name, <<"phone">>, Phone, <<"avatar">>, Avatar] ->
            UserId = erlang:binary_to_integer(UserIdBin),
            MemberToml = {<<"member">>, [{<<"id">>, UserId},
                                         {<<"name">>, Name},
                                         {<<"phone">>, Phone},
                                         {<<"avatar">>, Avatar}]},
            unpack_ssdb(T, [MemberToml|Toml]);
        _ ->
            unpack_ssdb(T, Toml)
    end;
unpack_ssdb([], Toml) ->
    {ok, Toml}.



unpack_odbc(ODBCResult) ->
    unpack_odbc(ODBCResult, []).
unpack_odbc([{UserId, Name, Phone, Avatar}|T], Toml) ->
    MemberToml = {<<"member">>, [{<<"id">>, UserId},
                                 {<<"name">>, Name},
                                 {<<"phone">>, Phone},
                                 {<<"avatar">>, Avatar}]},
    unpack_odbc(T, [MemberToml|Toml]);
unpack_odbc([], Toml) ->
    {ok, Toml}.