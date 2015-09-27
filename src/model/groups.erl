%% ===================================================================
%% Author xiaotie
%% 2015-9-14
%% groups data logic module
%% ===================================================================

-module (groups).

-export ([create/3, add_members/2, add_member/2, get_user_id_list/1]).

-compile (export_all).



%% ===================================================================
%% API functions
%% ===================================================================

create(Name, CreaterId, Members) ->
    {ok, _, [{GroupId}]} = postgresql:exec(<<"select nextval('groups_id_seq');">>),
    InsertStr = <<"insert into groups values($1, $2, $3, now(), now());">>,
    {ok, 1} = postgresql:exec(InsertStr, [GroupId, Name, CreaterId]),
    add_members(GroupId, [CreaterId|Members]).


add_members(GroupId, [H|T]) ->
    ok = add_member(GroupId, H),
    add_members(GroupId, T);
add_members(_, []) ->
    ok.


add_member(GroupId, UserId) ->
    InsertStr = <<"insert into group_members values($1, $2, now(), now());">>,
    {ok, 1} = postgresql:exec(InsertStr, [GroupId, UserId]),
    ok.


get_user_id_list(GroupId) ->
    QueryStr = <<"select user_id from group_members where group_id = $1">>,
    {ok, _, UserIdList} = postgresql:exec(QueryStr, [GroupId]),
    {ok, UserIdList}.



%% ===================================================================
%% Internal functions
%% ===================================================================