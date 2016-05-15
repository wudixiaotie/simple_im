%% ===================================================================
%% Author xiaotie
%% 2015-9-14
%% users data logic module
%% ===================================================================

-module(users).

-export([create/3, verify/2, find/1, to_toml/1]).



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
            SQL1 = <<"SELECT id FROM users where phone = $1;">>,
            {ok, _, [{Id}]} = postgresql:exec(SQL1, [Phone]),
            IdBin = erlang:integer_to_binary(Id),
            [<<"ok">>, _] = ssdb:q([<<"multi_hset">>, <<"users_", IdBin/binary>>,
                                    <<"name">>, Name,
                                    <<"phone">>, Phone,
                                    <<"password">>, EncryptedPassword,
                                    <<"salt">>, Salt,
                                    <<"avatar">>, <<>>]),
            [<<"ok">>, _] = ssdb:q([<<"multi_hset">>, <<"users_phone_", Phone/binary>>,
                                    <<"name">>, Name,
                                    <<"id">>, IdBin,
                                    <<"password">>, EncryptedPassword,
                                    <<"salt">>, Salt,
                                    <<"avatar">>, <<>>]),
            ok;
        1 ->
            {error, user_exist};
        _ ->
            {error, unknown}
    end.


verify(Phone, Password) ->
    case ssdb:q([<<"multi_hget">>, <<"users_phone_", Phone/binary>>, <<"id">>, <<"password">>, <<"salt">>]) of
        [<<"ok">>, <<"id">>, IdBin, <<"password">>, EncryptedPassword, <<"salt">>, Salt] ->
            UserId = erlang:binary_to_integer(IdBin),
            verify(UserId, Password, EncryptedPassword, Salt);
        [<<"ok">>] ->
            {error, user_does_not_exist};
        _ ->
            log:e("[SSDB] verify error phone:~p, password:~p!~n", [Phone, Password]),
            SQL = <<"SELECT id, password, salt FROM users WHERE phone = $1;">>,
            case postgresql:exec(SQL, [Phone]) of
                {ok, _, []} ->
                    {error, user_does_not_exist};
                {ok, _, [{UserId, EncryptedPassword, Salt}]} ->
                    verify(UserId, Password, EncryptedPassword, Salt);
                {error, Reason} ->
                    {error, Reason}
            end
    end.


find({id, Id}) ->
    IdBin = erlang:integer_to_binary(Id),
    case ssdb:q([<<"multi_hget">>, <<"users_", IdBin/binary>>, <<"name">>, <<"phone">>, <<"avatar">>]) of
        [<<"ok">>, <<"name">>, Name, <<"phone">>, Phone, <<"avatar">>, Avatar] ->
            {ok, [{Id, Name, Phone, Avatar}]};
        _ ->
            log:e("[SSDB] find id error id:~p!~n", [Id]),
            SQL = <<"SELECT id, name, phone, avatar FROM users WHERE id = $1;">>,
            {ok, _, Result} = postgresql:exec(SQL, [Id]),
            {ok, Result}
    end;
find({phone, Phone}) ->
    case ssdb:q([<<"multi_hget">>, <<"users_phone_", Phone/binary>>, <<"id">>, <<"name">>, <<"avatar">>]) of
        [<<"ok">>, <<"id">>, IdBin, <<"name">>, Name, <<"avatar">>, Avatar] ->
            Id = erlang:binary_to_integer(IdBin),
            {ok, [{Id, Name, Phone, Avatar}]};
        _ ->
            log:e("[SSDB] find phone error phone:~p!~n", [Phone]),
            SQL = <<"SELECT id, name, phone, avatar FROM users WHERE phone = $1;">>,
            {ok, _, Result} = postgresql:exec(SQL, [Phone]),
            {ok, Result}
    end.


to_toml(TupleList) ->
    to_toml(TupleList, []).


%% ===================================================================
%% Internal functions
%% ===================================================================

verify(UserId, Password, EncryptedPassword, Salt) ->
    case utility:md5_hex_32(<<Password/binary, Salt/binary>>) of
        {ok, EncryptedPassword} ->
            {ok, true, UserId};
        _ ->
            {ok, false}
    end.    


to_toml([{Id, Name, Phone, Avatar}|T], Toml) ->
    UserToml = {<<"user">>, [{<<"id">>, Id},
                             {<<"name">>, Name},
                             {<<"phone">>, Phone},
                             {<<"avatar">>, Avatar}]},
    to_toml(T, [UserToml|Toml]);
to_toml([], Toml) ->
    {ok, Toml}.