%% ===================================================================
%% Author xiaotie
%% 2015-9-14
%% users data logic module
%% ===================================================================

-module (users).

-export ([create/3, verify/2, find/1, add_contact/2,
          delete_contact/2, find_contacts/1,
          find_contacts/2, parse/1]).

-include("user.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

create(Name, Phone, Password) ->
    {ok, Salt} = utility:random_binary_16(),
    {ok, EncryptedPassword} = utility:md5_hex_32(<<Password/binary, Salt/binary>>),
    SQL = <<"INSERT INTO users(name,
                               phone,
                               password,
                               salt,
                               contact_version,
                               updated_at,
                               created_at)
             VALUES($1, $2, $3, $4, 0, now(), now());">>,
    {ok, 1} = postgresql:exec(SQL, [Name, Phone, EncryptedPassword, Salt]),
    ok.


verify(Phone, Password) ->
    SQL = <<"SELECT id, password, salt FROM users WHERE phone = $1;">>,
    case postgresql:exec(SQL, [Phone]) of
        {ok, _, []} ->
            {error, user_does_not_exist};
        {ok, _, [{UserId, EncryptedPassword, Salt}]} ->
            case utility:md5_hex_32(<<Password/binary, Salt/binary>>) of
                {ok, EncryptedPassword} ->
                    {ok, true, UserId};
                _ ->
                    {ok, false}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


find({phone, Phone}) ->
    SQL = <<"SELECT id, name FROM users WHERE phone = $1;">>,
    {ok, _, Result} = postgresql:exec(SQL, [Phone]),
    {ok, Result}.


add_contact(UserId, ContactId) ->
    SQL = <<"SELECT add_contact($1, $2);">>,
    {ok, _} = postgresql:exec(SQL, [UserId, ContactId]),
    ok.


delete_contact(UserId, ContactId) ->
    SQL = <<"SELECT delete_contact($1, $2);">>,
    {ok, _} = postgresql:exec(SQL, [UserId, ContactId]),
    ok.


find_contacts(UserId) ->
    SQL = <<"SELECT contact_id FROM contacts WHERE user_id = $1;">>,
    {ok, _, Result} = postgresql:exec(SQL, [UserId]),
    unpack(Result).


find_contacts(UserId, OldVersion) ->
    SQL = <<"SELECT contact_id FROM contacts
             WHERE user_id = $1 AND contact_version > $2;">>,
    {ok, _, Result} = postgresql:exec(SQL, [UserId, OldVersion]),
    unpack(Result).


% [{<<"device">>,<<"android">>},{<<"id">>,<<"1">>},{<<"phone">>, <<"18501260698">>}]
parse(TupleList) ->
    parse(TupleList, #user{}).



%% ===================================================================
%% Internal functions
%% ===================================================================

unpack(TupleList) ->
    unpack(TupleList, []).
unpack([{Value}|T], Result) ->
    unpack(T, [Value|Result]);
unpack([], Result) ->
    {ok, Result}.


parse([{<<"id">>, IdBin}|T], User) when is_binary(IdBin) ->
    Id = erlang:binary_to_integer(IdBin),
    parse(T, User#user{id = Id});
parse([{<<"id">>, Id}|T], User) ->
    parse(T, User#user{id = Id});
parse([{<<"device">>, Device}|T], User) ->
    parse(T, User#user{device = Device});
parse([{<<"token">>, Token}|T], User) ->
    parse(T, User#user{token = Token});
parse([{<<"phone">>, Phone}|T], User) ->
    parse(T, User#user{phone = Phone});
parse([{<<"password">>, Password}|T], User) ->
    parse(T, User#user{password = Password});
parse([_|T], User) ->
    parse(T, User);
parse([], User) ->
    {ok, User}.