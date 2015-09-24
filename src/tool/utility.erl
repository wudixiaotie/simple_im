%% ===================================================================
%% Author xiaotie
%% 2015-9-12
%% utility for simple im
%% ===================================================================

-module (utility).

-export ([tuple_to_toml/1, md5_hex_32/1, random_binary_16/0, random_number/1, guid/0]).



%% ===================================================================
%% APIs
%% ===================================================================

tuple_to_toml({Name, Attrs}) ->
    AttrsRev = lists:reverse(Attrs),
    tuple_to_toml(Name, AttrsRev, <<"[", Name/binary, "]">>).
tuple_to_toml(Name, [H|T], Result) ->
    {Key, Value} = H,
    case is_list(Value) of
        true ->
            {ok, Bin} = list_to_toml(Value),
            NewResult = <<Result/binary, " [", Name/binary, ".", Key/binary, "]", Bin/binary>>;
        _ ->
            {ok, Bin} = key_value_to_toml({Key, Value}),
            NewResult = <<Result/binary, " ", Bin/binary>>
    end,
    tuple_to_toml(Name, T, NewResult);
tuple_to_toml(_, _, Result) ->
    {ok, Result}.


md5_hex_32(Bin) ->
    MD5_16 = erlang:md5(Bin),
    Md5 = << <<(hex(A)), (hex(B))>> || <<A:4,B:4>> <= MD5_16 >>,
    {ok, Md5}.


random_binary_16() ->
    {ok, base64:encode(crypto:strong_rand_bytes(12))}.


random_number(Max) ->
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes (12),
    random:seed (A, B, C),
    {ok, random:uniform(Max)}.


guid() ->
    RandomBytes = crypto:strong_rand_bytes(6),
    TimestampBytes = erlang:md5(erlang:term_to_binary(os:timestamp())),
    {ok, base64:encode(<<RandomBytes/binary, TimestampBytes/binary>>)}.



%% ===================================================================
%% Internal functions
%% ===================================================================

list_to_toml(List) ->
    ListRev = lists:reverse(List),
    list_to_toml(ListRev, <<"">>).
list_to_toml([{Key, Value}|T], Result) ->
    {ok, Bin} = key_value_to_toml({Key, Value}),
    list_to_toml(T, <<Result/binary, " ", Bin/binary>>);
list_to_toml([], Result) ->
    {ok, Result}.

key_value_to_toml({Key, Value}) when is_integer(Value) ->
    ValueBin = integer_to_binary(Value),
    {ok, <<Key/binary, " = ", ValueBin/binary, "">>};
key_value_to_toml({Key, Value}) ->
    {ok, <<Key/binary, " = \"", Value/binary, "\"">>}.


hex(0)  -> $0;
hex(1)  -> $1;
hex(2)  -> $2;
hex(3)  -> $3;
hex(4)  -> $4;
hex(5)  -> $5;
hex(6)  -> $6;
hex(7)  -> $7;
hex(8)  -> $8;
hex(9)  -> $9;
hex(10) -> $a;
hex(11) -> $b;
hex(12) -> $c;
hex(13) -> $d;
hex(14) -> $e;
hex(15) -> $f.