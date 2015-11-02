%% ===================================================================
%% Author xiaotie
%% 2015-9-14
%% groups data logic module
%% ===================================================================

-module (group_members).

-export ([create/3, find/1, delete/2]).



%% ===================================================================
%% API functions
%% ===================================================================

create(GroupId, Key, UserId) ->
    SQL = <<"SELECT create_group_member($1, $2, $3);">>,
    {ok, _, [{Result}]} = postgresql:exec(SQL, [GroupId, Key, UserId]),
    case Result of
        0 ->
            ok;
        1 ->
            {error, unauthorized};
        _ ->
            {error, unknown}
    end.


find({group_id, GroupId}) ->
    SQL = <<"SELECT user_id FROM group_members WHERE group_id = $1;">>,
    {ok, _, UserIdList} = postgresql:exec(SQL, [GroupId]),
    utility:unpack(UserIdList);
find({user_id, UserId}) ->
    SQL = <<"SELECT group_id FROM group_members WHERE user_id = $1;">>,
    {ok, _, GroupIdList} = postgresql:exec(SQL, [UserId]),
    utility:unpack(GroupIdList).


delete(GroupId, UserId) ->
    SQL = <<"DELETE FROM group_members WHERE group_id = $1 AND user_id = $2;">>,
    {ok, _} = postgresql:exec(SQL, [GroupId, UserId]),
    ok.




%% ===================================================================
%% Internal functions
%% ===================================================================