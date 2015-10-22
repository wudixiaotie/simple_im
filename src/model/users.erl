%% ===================================================================
%% Author xiaotie
%% 2015-9-14
%% users data logic module
%% ===================================================================

-module (users).

-export ([create/3, verify/2, find/1, add_contact/2,
          find_contacts/1, parse/1]).

-include("user.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

create(Name, Phone, Password) ->
    {ok, Salt} = utility:random_binary_16(),
    {ok, EncryptedPassword} = utility:md5_hex_32(<<Password/binary, Salt/binary>>),
    InsertSQL = <<"INSERT INTO users(name,
                                     phone,
                                     password,
                                     salt,
                                     contact_version,
                                     updated_at,
                                     created_at)
                   VALUES($1, $2, $3, $4, 0, now(), now());">>,
    {ok, 1} = postgresql:exec(InsertSQL, [Name, Phone, EncryptedPassword, Salt]),
    ok.


verify(Phone, Password) ->
    QuerySQL = <<"SELECT id, password, salt FROM users WHERE phone = $1;">>,
    case postgresql:exec(QuerySQL, [Phone]) of
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
    QuerySQL = <<"SELECT id, name FROM users WHERE phone = $1;">>,
    {ok, _, Result} = postgresql:exec(QuerySQL, [Phone]),
    {ok, Result}.


add_contact(UserId, ContactId) ->
    InsertSQL = <<"INSERT INTO contacts VALUES($1, $2, now(), now());">>,
    {ok, 1} = postgresql:exec(InsertSQL, [UserId, ContactId]),
    ok.


find_contacts(UserId) ->
    QuerySQL = <<"SELECT friend_user_id FROM user_relations WHERE user_id = $1;">>,
    {ok, _, Result} = postgresql:exec(QuerySQL, [UserId]),
    unpack(Result).


delete_contact(UserId, ContactId) ->
    DeleteSQL = <<"DELETE FROM user_relations WHERE user_id = $1 AND friend_user_id = $2;">>,
    {ok, 1} = postgresql:exec(DeleteSQL, [UserId, ContactId]),
    ok.


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