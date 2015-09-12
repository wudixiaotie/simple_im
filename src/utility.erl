-module (utility).

-export ([tuple_to_toml/1]).

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


list_to_toml(List) ->
    ListRev = lists:reverse(List),
    list_to_toml(ListRev, <<"">>).
list_to_toml([{Key, Value}|T], Result) ->
    {ok, Bin} = key_value_to_toml({Key, Value}),
    list_to_toml(T, <<Result/binary, " ", Bin/binary>>);
list_to_toml([], Result) ->
    {ok, Result}.

key_value_to_toml({Key, Value}) ->
    {ok, <<Key/binary, " = \"", Value/binary, "\"">>}.