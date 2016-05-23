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
    CreatedAt = utility:timestamp(),
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
    CreatedAt = utility:timestamp(),
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


find({group_id, GroupId}) ->
    % GroupIdBin = erlang:integer_to_binary(GroupId),
    % case ssdb:q([<<"zscan">>, <<"group_members_", GroupIdBin, <<>>, <<>>, <<>>, <<"-1">>]) of
    %     [<<"ok">>|MemberIdList] ->
    %         {ok, ContactsList} = find_contact_info(ContactIdList),
    %         {ok, ContactsList};
    %     _ ->
    %         log:e("[SSDB] contacts: find error id:~p version:0!~n", [UserId]),
    %         SQL = <<"SELECT u.id,
    %                         u.name,
    %                         u.phone,
    %                         u.avatar
    %                  FROM users u, contacts c
    %                  WHERE u.id = c.contact_id
    %                  AND c.user_id = $1;">>,
    %         {ok, _, Result} = postgresql:exec(SQL, [UserId]),
    %         {ok, Result}
    % end;
    %     [<<"ok">>, ]
    % end;
    SQL = <<"SELECT user_id FROM group_members WHERE group_id = $1;">>,
    {ok, _, UserIdList} = postgresql:exec(SQL, [GroupId]),
    unpack(UserIdList);
find({user_id, UserId}) ->
    SQL = <<"SELECT group_id FROM group_members WHERE user_id = $1;">>,
    {ok, _, GroupIdList} = postgresql:exec(SQL, [UserId]),
    unpack(GroupIdList).


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


unpack(TupleList) ->
    unpack(TupleList, []).
unpack([{Value}|T], Result) ->
    unpack(T, [Value|Result]);
unpack([], Result) ->
    {ok, lists:reverse(Result)}.