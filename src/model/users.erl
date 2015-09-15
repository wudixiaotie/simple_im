%% ===================================================================
%% Author xiaotie
%% 2015-9-14
%% users data logic module
%% ===================================================================

-module (users).

-export ([create/3, verify/2]).

create(Name, Phone, Password) ->
    {ok, Salt} = utility:random_binary_16(),
    {ok, EncryptedPassword} = utility:md5_hex_32(<<Password/binary, Salt/binary>>),
    InsertStr = <<"insert into users(name, phone, password, salt, updated_at, created_at) ",
                   "values($1, $2, $3, $4, now(), now());">>,
    {ok, 1} = postgresql:exec(InsertStr, [Name, Phone, EncryptedPassword, Salt]),
    ok.

verify(Phone, Password) ->
    QueryStr = <<"select id, password, salt from users where phone = $1">>,
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