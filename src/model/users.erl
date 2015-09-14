%% ===================================================================
%% Author xiaotie
%% 2015-9-14
%% users data logic module
%% ===================================================================

-module (users).

-export ([create/3]).

create(Name, Phone, Password) ->
    {ok, Salt} = utility:random_binary_16(),
    {ok, EncryptPassword} = utility:md5_hex_32(<<Password/binary, Salt/binary>>),
    InsertStr = <<"insert into users(name, phone, password, salt, updated_at, created_at) ",
                   "values($1, $2, $3, $4, now(), now());">>,
    {ok, 1} = postgresql:exec(InsertStr, [Name, Phone, EncryptPassword, Salt]),
    ok.