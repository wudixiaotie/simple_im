%% ===================================================================
%% Author xiaotie
%% 2015-9-14
%% groups data logic module
%% ===================================================================

-module (groups).

-export ([create/3, delete/1]).



%% ===================================================================
%% API functions
%% ===================================================================

create(Name, CreatorId, Members) ->
    {ok, MembersBin} = utility:join(Members, <<",">>),
    SQL = <<"SELECT create_group($1, $2, '{", MembersBin/binary, "}');">>,
    {ok, _, [{GroupId}]} = postgresql:exec(SQL, [Name, CreatorId]),
    {ok, GroupId}.


delete(GroupId) ->
    SQL = <<"DELETE FROM groups WHERE id = $1;">>,
    {ok, _} = postgresql:exec(SQL, [GroupId]),
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================