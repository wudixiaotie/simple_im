-module (users).

-export ([create/3]).

create(Name, Phone, Password) ->
    {ok, Salt} = utility:random_binary_16(),
    {ok, EncryptPassword} = utility:md5_hex_32(<<Password/binary, Salt/binary>>).
