%% ===================================================================
%% Author xiaotie
%% 2015-9-14
%% users data logic module
%% ===================================================================

-module(contacts).

-export([create/2, find/2, delete/2]).



%% ===================================================================
%% APIs
%% ===================================================================

create(AUserId, BUserId)
    when is_integer(AUserId), is_integer(BUserId) ->
    SQL = <<"SELECT create_contact($1, $2);">>,
    {ok, _, [{Result}]} = postgresql:exec(SQL, [AUserId, BUserId]),
    case Result of
        0 ->
            ok;
        1 ->
            {error, unauthorized};
        _ ->
            {error, unknown}
    end.


delete(AUserId, BUserId)
    when is_integer(AUserId), is_integer(BUserId) ->
    SQL = <<"SELECT delete_contact($1, $2);">>,
    {ok, _, _} = postgresql:exec(SQL, [AUserId, BUserId]),
    ok.


find(UserId, 0) when is_integer(UserId) ->
    SQL = <<"SELECT u.id,
                    u.name,
                    u.phone,
                    u.avatar
             FROM users u, contacts c
             WHERE u.id = c.contact_id
             AND c.user_id = $1;">>,
    {ok, _, Result} = postgresql:exec(SQL, [UserId]),

    SQLVersion = <<"SELECT contact_version FROM users WHERE id = $1">>,
    {ok, _, [{CurrentVersion}]} = postgresql:exec(SQLVersion, [UserId]),
    {ok, CurrentVersion, Result};
find(UserId, ContactVersion)
    when is_integer(UserId), is_integer(ContactVersion) ->
    SQL = <<"SELECT u.id,
                    u.name,
                    u.phone,
                    u.avatar
             FROM users u, contacts c
             WHERE u.id = c.contact_id
             AND c.user_id = $1
             AND c.contact_version > $2;">>,
    {ok, _, Result} = postgresql:exec(SQL, [UserId, ContactVersion]),

    SQLVersion = <<"SELECT contact_version FROM users WHERE id = $1">>,
    {ok, _, [{CurrentVersion}]} = postgresql:exec(SQLVersion, [UserId]),
    {ok, CurrentVersion, Result}.



%% ===================================================================
%% Internal functions
%% ===================================================================