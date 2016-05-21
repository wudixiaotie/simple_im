-module (ssdb_recover).

-export ([start/0]).


start() ->
    ssdb:start_link(),
    case postgresql:exec(<<"SELECT id, name, phone, avatar, password, salt FROM users">>) of
        {ok, _, List} ->
            ok = store_ssdb(List),
            ok;
        _ ->
            ok
    end.


store_ssdb([{UserId, Name, Phone, Avatar, EncryptedPassword, Salt}|T]) ->
    UserIdBin = erlang:integer_to_binary(UserId),
    [<<"ok">>, _] = ssdb:q([<<"multi_hset">>, <<"users_", UserIdBin/binary>>,
                            <<"name">>, Name,
                            <<"phone">>, Phone,
                            <<"password">>, EncryptedPassword,
                            <<"salt">>, Salt,
                            <<"avatar">>, Avatar]),
    [<<"ok">>, _] = ssdb:q([<<"multi_hset">>, <<"users_phone_", Phone/binary>>,
                            <<"name">>, Name,
                            <<"id">>, UserIdBin,
                            <<"password">>, EncryptedPassword,
                            <<"salt">>, Salt,
                            <<"avatar">>, Avatar]),
    store_ssdb(T);
store_ssdb([]) ->
    ok.