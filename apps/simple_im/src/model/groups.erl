%% ===================================================================
%% Author xiaotie
%% 2015-09-14
%% groups data logic module
%% ===================================================================

-module(groups).

-export([create/3, delete/2, find/1, find_members/1]).

-include("group.hrl").



%% ===================================================================
%% API functions
%% ===================================================================

create(Name, CreatorId, Members) ->
    {ok, MembersBin} = utility:join(Members, <<",">>),
    {ok, Key} = utility:random_binary_16(),
    CreatedAt = erlang:system_time(seconds),
    SQL = <<"SELECT create_group($1, $2, $3, $4, '{", MembersBin/binary, "}');">>,
    {ok, _, [{GroupId}]} = postgresql:exec(SQL, [Name, CreatorId, Key, CreatedAt]),

    GroupIdBin = erlang:integer_to_binary(GroupId),
    CreatorIdBin = erlang:integer_to_binary(CreatorId),
    CreatedAtBin = erlang:integer_to_binary(CreatedAt),
    [<<"ok">>, _] = ssdb:q([<<"multi_hset">>, <<"groups_", GroupIdBin/binary>>,
                            <<"name">>, Name,
                            <<"creator_id">>, CreatorIdBin,
                            <<"key">>, Key,
                            <<"created_at">>, CreatedAtBin]),
    [<<"ok">>, _] = ssdb:q([<<"zset">>, <<"user_groups_", CreatorIdBin/binary>>,
                            GroupIdBin, CreatedAtBin]),
    [<<"ok">>, _] = ssdb:q([<<"zset">>, <<"groups_members_", GroupIdBin/binary>>,
                            CreatorIdBin, CreatedAtBin]),
    {ok, GroupId, Key}.


delete(GroupId, CreatorId) ->
    SQL = <<"SELECT delete_group($1, $2);">>,
    {ok, _, [{Result}]} = postgresql:exec(SQL, [GroupId, CreatorId]),
    case Result of
        0 ->
            GroupIdBin = erlang:integer_to_binary(GroupId),
            [<<"ok">>, _] = ssdb:q([<<"hclear">>, <<"groups_", GroupIdBin/binary>>]),
            [<<"ok">>|GroupMembers] = ssdb:q([<<"zscan">>, <<"groups_members_", GroupIdBin/binary>>, <<>>, <<>>, <<>>, <<"-1">>]),
            ok = delete_ssdb(GroupMembers, GroupIdBin),
            ok;
        1 ->
            {error, unauthorized};
        _ ->
            {error, unknown}
    end.


find({group_id, GroupId}) ->
    GroupIdBin = erlang:integer_to_binary(GroupId),
    case catch ssdb:q([<<"multi_hget">>, <<"groups_", GroupIdBin/binary>>, <<"name">>, <<"creator_id">>, <<"key">>, <<"created_at">>]) of
        [<<"ok">>, <<"name">>, Name, <<"creator_id">>, CreatorIdBin,
         <<"key">>, Key, <<"created_at">>, CreatedAtBin] ->
            CreatorId = erlang:binary_to_integer(CreatorIdBin),
            CreatedAt = erlang:binary_to_integer(CreatedAtBin),
            {ok, {group, GroupId, Name, CreatorId, Key, CreatedAt}};
        _ ->
            log:e("[SSDB] groups: find error id:~p~n", [GroupId]),
            SQL = <<"SELECT name, creator_id, key, extract(epoch from created_at)::integer
                     FROM groups WHERE id = $1;">>,
            case postgresql:exec(SQL, [GroupId]) of
                {ok, _, []} ->
                    {error, null};
                {ok, _, [{Name, CreatorId, Key, CreatedAt}]} ->
                    {ok, {group, GroupId, Name, CreatorId, Key, CreatedAt}}
            end
    end.


find_members(GroupId) ->
    GroupIdBin = erlang:integer_to_binary(GroupId),
    case catch ssdb:q([<<"zscan">>, <<"group_members_", GroupIdBin/binary>>, <<>>, <<>>, <<>>, <<"-1">>]) of
        [<<"ok">>|SSDBResult] ->
            {ok, MemberIdList} = unpack_ssdb(SSDBResult),
            {ok, MemberIdList};
        _ ->
            log:e("[SSDB] groups: find_members error id:~p~n", [GroupId]),
            SQL = <<"SELECT user_id FROM group_members WHERE group_id = $1;">>,
            {ok, _, ODBCResult} = postgresql:exec(SQL, [GroupId]),
            {ok, MemberIdList} = unpack_odbc(ODBCResult),
            {ok, MemberIdList}
    end.



%% ===================================================================
%% Internal functions
%% ===================================================================

delete_ssdb([UserIdBin, _|T], GroupIdBin) ->
    [<<"ok">>, _] = ssdb:q([<<"zdel">>, <<"user_groups_", UserIdBin/binary>>, GroupIdBin]),
    delete_ssdb(T, GroupIdBin);
delete_ssdb([], GroupIdBin) ->
    [<<"ok">>, _] = ssdb:q([<<"zclear">>, <<"groups_members_", GroupIdBin/binary>>]),
    ok.


unpack_ssdb(SSDBResult) ->
    unpack_ssdb(SSDBResult, []).
unpack_ssdb([MemberIdBin, _|T], MemberIdList) ->
    MemberId = erlang:binary_to_integer(MemberIdBin),
    unpack_ssdb(T, [MemberId|MemberIdList]);
unpack_ssdb([], MemberIdList) ->
    {ok, MemberIdList}.


unpack_odbc(ODBCResult) ->
    unpack_odbc(ODBCResult, []).
unpack_odbc([{MemberId}|T], MemberIdList) ->
    unpack_odbc(T, [MemberId|MemberIdList]);
unpack_odbc([], MemberIdList) ->
    {ok, MemberIdList}.