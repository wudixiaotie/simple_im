%% ===================================================================
%% Author xiaotie
%% 2015-9-14
%% users data logic module
%% ===================================================================

-module (users).

-export ([create/3, verify/2, find/1, parse/1, to_toml/1]).

-include("user.hrl").



%% ===================================================================
%% APIs
%% ===================================================================

create(Name, Phone, Password) ->
    {ok, Salt} = utility:random_binary_16(),
    {ok, EncryptedPassword} = utility:md5_hex_32(<<Password/binary, Salt/binary>>),
    SQL = <<"SELECT create_user($1, $2, $3, $4);">>,
    {ok, _, [{Result}]} = postgresql:exec(SQL, [Name,
                                                Phone,
                                                EncryptedPassword,
                                                Salt]),
    case Result of
        0 ->
            ok;
        1 ->
            {error, user_exist};
        _ ->
            {error, unknown}
    end.


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


find({id, Id}) ->
    SQL = <<"SELECT id, name, phone, avatar FROM users WHERE id = $1;">>,
    {ok, _, Result} = postgresql:exec(SQL, [Id]),
    {ok, Result};
find({phone, Phone}) ->
    SQL = <<"SELECT id, name, phone, avatar FROM users WHERE phone = $1;">>,
    {ok, _, Result} = postgresql:exec(SQL, [Phone]),
    {ok, Result}.


parse(TupleList) ->
    parse(TupleList, #user{}).


to_toml(TupleList) ->
    to_toml(TupleList, []).


%% ===================================================================
%% Internal functions
%% ===================================================================

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


to_toml([{Id, Name, Phone, Avatar}|T], Toml) ->
    UserToml = {<<"user">>, [{<<"id">>, Id},
                             {<<"name">>, Name},
                             {<<"phone">>, Phone},
                             {<<"avatar">>, Avatar}]},
    to_toml(T, [UserToml|Toml]);
to_toml([], Toml) ->
    {ok, Toml}.