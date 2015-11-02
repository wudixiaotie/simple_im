%% ===================================================================
%% Author xiaotie
%% 2015-9-14
%% groups data logic module
%% ===================================================================

-module (groups).

-export ([create/3, delete/2]).



%% ===================================================================
%% API functions
%% ===================================================================

create(Name, CreatorId, Members) ->
    {ok, MembersBin} = utility:join(Members, <<",">>),
    {ok, Key} = utility:random_binary_16(),
    SQL = <<"SELECT create_group($1, $2, $3, '{", MembersBin/binary, "}');">>,
    {ok, _, [{GroupId}]} = postgresql:exec(SQL, [Name, CreatorId, Key]),
    {ok, GroupId, Key}.


delete(GroupId, CreatorId) ->
    SQL = <<"SELECT delete_group($1, $2);">>,
    {ok, _, [{Result}]} = postgresql:exec(SQL, [GroupId, CreatorId]),
    case Result of
        0 ->
            ok;
        1 ->
            {error, unauthorized};
        _ ->
            {error, unknown}
    end.



%% ===================================================================
%% Internal functions
%% ===================================================================