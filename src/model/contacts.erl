%% ===================================================================
%% Author xiaotie
%% 2015-9-14
%% users data logic module
%% ===================================================================

-module (contacts).

-export ([create/2, find/2, remove/2]).



%% ===================================================================
%% APIs
%% ===================================================================

create(UserId, ContactId) ->
    SQL = <<"SELECT add_contact($1, $2);">>,
    {ok, _} = postgresql:exec(SQL, [UserId, ContactId]),
    ok.


remove(UserId, ContactId) ->
    SQL = <<"SELECT delete_contact($1, $2);">>,
    {ok, _} = postgresql:exec(SQL, [UserId, ContactId]),
    ok.


find(UserId, Bin) when is_binary(Bin) ->
    ContactVersion = erlang:binary_to_integer(Bin),
    find(UserId, ContactVersion);
find(UserId, 0) ->
    SQL = <<"SELECT contact_id FROM contacts WHERE user_id = $1;">>,
    {ok, _, Result} = postgresql:exec(SQL, [UserId]),
    unpack(Result);
find(UserId, ContactVersion) ->
    SQL = <<"SELECT contact_id FROM contacts
             WHERE user_id = $1 AND contact_version > $2;">>,
    {ok, _, Result} = postgresql:exec(SQL, [UserId, ContactVersion]),
    unpack(Result).



%% ===================================================================
%% Internal functions
%% ===================================================================

unpack(TupleList) ->
    unpack(TupleList, []).
unpack([{Value}|T], Result) ->
    unpack(T, [Value|Result]);
unpack([], Result) ->
    {ok, Result}.