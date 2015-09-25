%% ===================================================================
%% Author xiaotie
%% 2015-9-14
%% users data logic module
%% ===================================================================

-module (users).

-export ([create/3, verify/2, find/1, add_friend/2,
          get_friend_user_id_list/1, parse/1]).

-include("user.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

create(Name, Phone, Password) ->
    {ok, Salt} = utility:random_binary_16(),
    {ok, EncryptedPassword} = utility:md5_hex_32(<<Password/binary, Salt/binary>>),
    InsertStr = <<"insert into users(name, phone, password, salt, updated_at, created_at) ",
                   "values($1, $2, $3, $4, now(), now());">>,
    {ok, 1} = postgresql:exec(InsertStr, [Name, Phone, EncryptedPassword, Salt]),
    ok.


verify(Phone, Password) ->
    QueryStr = <<"select id, password, salt from users where phone = $1;">>,
    case postgresql:exec(QueryStr, [Phone]) of
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
    QueryStr = <<"select id, name from users where phone = $1;">>,
    {ok, _, [Result]} = postgresql:exec(QueryStr, [Phone]),
    {ok, Result}.


add_friend(UserId, FriendUserId) ->
    InsertStr = <<"insert into user_relations values($1, $2, now(), now());">>,
    {ok, 1} = postgresql:exec(InsertStr, [UserId, FriendUserId]),
    ok.


get_friend_user_id_list(UserId) ->
    QueryStr = <<"select friend_user_id from user_relations where user_id = $1;">>,
    {ok, _, Result} = postgresql:exec(QueryStr, [UserId]),
    unpack(Result).


delete_friend(UserId, FriendUserId) ->
    DeleteStr = <<"delete from user_relations where user_id = $1 and friend_user_id = $2;">>,
    {ok, 1} = postgresql:exec(DeleteStr, [UserId, FriendUserId]),
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
parse([], User) ->
    {ok, User}.