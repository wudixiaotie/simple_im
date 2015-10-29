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
    {ok, MembersStr} = utility:join(Members, ","),
    MembersBin = erlang:list_to_binary(MembersStr),
    SQL = <<"SELECT create_group($1, $2, '{", MembersBin/binary, "}');">>,
    {ok, _, _} = postgresql:exec(SQL, [Name, CreatorId]),
    ok.


delete(GroupId) ->
    SQL = <<"DELETE FROM groups WHERE id = $1;">>,
    {ok, _} = postgresql:exec(SQL, [GroupId]),
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================