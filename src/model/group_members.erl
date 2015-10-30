%% ===================================================================
%% Author xiaotie
%% 2015-9-14
%% groups data logic module
%% ===================================================================

-module (group_members).

-export ([create/2, find/1, delete/2]).



%% ===================================================================
%% API functions
%% ===================================================================

create(GroupId, UserIdList) when is_list(UserIdList) ->
    {ok, MembersBin} = utility:join(UserIdList, <<",">>),
    SQL = <<"SELECT create_group_members($1, '{", MembersBin/binary, "}');">>,
    {ok, _, _} = postgresql:exec(SQL, [GroupId]),
    ok;
create(GroupId, UserId) when is_integer(UserId) ->
    InsertStr = <<"INSERT INTO group_members VALUES($1, $2, now(), now());">>,
    {ok, 1} = postgresql:exec(InsertStr, [GroupId, UserId]),
    ok.


find({group_id, GroupId}) ->
    QueryStr = <<"SELECT user_id FROM group_members WHERE group_id = $1;">>,
    {ok, _, UserIdList} = postgresql:exec(QueryStr, [GroupId]),
    {ok, UserIdList};
find({user_id, UserId}) ->
    QueryStr = <<"SELECT group_id FROM group_members WHERE user_id = $1;">>,
    {ok, _, GroupIdList} = postgresql:exec(QueryStr, [UserId]),
    {ok, GroupIdList}.


delete(GroupId, UserId) ->
    SQL = <<"DELETE FROM group_members WHERE group_id = $1 AND user_id = $2;">>,
    {ok, _} = postgresql:exec(SQL, [GroupId, UserId]),
    ok.




%% ===================================================================
%% Internal functions
%% ===================================================================