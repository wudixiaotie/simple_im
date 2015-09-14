%% ===================================================================
%% Author xiaotie
%% 2015-9-14
%% groups data logic module
%% ===================================================================

-module (groups).

-export ([create/2, add_members/2, add_member/2]).

-compile (export_all).


%% ===================================================================
%% API functions
%% ===================================================================

create(Name, Members) ->
    {ok, _, [{GroupId}]} = postgresql:exec(<<"select nextval('groups_id_seq');">>),
    InsertStr = <<"insert into groups(id, name, updated_at, created_at) values($1, $2, now(), now());">>,
    {ok, 1} = postgresql:exec(InsertStr, [GroupId, Name]),
    add_members(GroupId, Members).


add_members(GroupId, Members) when is_integer(GroupId) ->
    GroupIdBin = erlang:integer_to_binary(GroupId),
    add_members(GroupIdBin, Members);
add_members(_, []) ->
    ok;
add_members(GroupIdBin, [H|T]) ->
    ok = add_member(GroupIdBin, H),
    add_members(GroupIdBin, T).


add_member(GroupId, UserId) when is_integer(GroupId) ->
    GroupIdBin = erlang:integer_to_binary(GroupId),
    add_member(GroupIdBin, UserId);
add_member(GroupIdBin, UserId) when is_integer(UserId) ->
    UserIdBin = erlang:integer_to_binary(UserId),
    add_member(GroupIdBin, UserIdBin);
add_member(GroupIdBin, UserIdBin) ->
    InsertStr = <<"insert into group_members(group_id, user_id, updated_at, created_at) values(",
                  GroupIdBin/binary, ", ", UserIdBin/binary, ", now(), now());">>,
    {ok, 1} = postgresql:exec(InsertStr),
    ok.