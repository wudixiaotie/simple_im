%% ===================================================================
%% Author xiaotie
%% 2015-10-6
%% toml helper for simple_im
%% ===================================================================

-module (toml).

-export ([term_2_binary/1, binary_2_term/1]).



%% ===================================================================
%% API functions
%% ===================================================================

term_2_binary(Tuple) when is_tuple(Tuple) ->
    tuple_2_binary(Tuple);
term_2_binary(List) when is_list(List) ->
    list_2_binary(List);
term_2_binary(_) ->
    {error, wrong_type}.


binary_2_term(Binary) when is_binary(Binary) ->
    List = erlang:binary_to_list(Binary),
    binary_2_term(List, []);
binary_2_term(List) ->
    binary_2_term(List, []).



%% ===================================================================
%% Internal functions
%% ===================================================================

tuple_2_binary({Name, Attrs}) ->
    TomlBin = <<"[[", Name/binary, "]]">>,
    tuple_2_binary(Name, Attrs, TomlBin, []).
tuple_2_binary(Name, [{Key, Binary}|T], TomlBin, ChildList) when is_binary(Binary) ->
    NewTomlBin = <<TomlBin/binary, " ", Key/binary, " = \"", Binary/binary, "\"">>,
    tuple_2_binary(Name, T, NewTomlBin, ChildList);
tuple_2_binary(Name, [{Key, Integer}|T], TomlBin, ChildList) when is_integer(Integer) ->
    Binary = erlang:integer_to_binary(Integer),
    NewTomlBin = <<TomlBin/binary, " ", Key/binary, " = ", Binary/binary>>,
    tuple_2_binary(Name, T, NewTomlBin, ChildList);
tuple_2_binary(Name, [{Key, [H|_] = List}|T], TomlBin, ChildList) when is_tuple(H) ->
    tuple_2_binary(Name, T, TomlBin, [{Key, List}|ChildList]);
tuple_2_binary(Name, [{Key, []}|T], TomlBin, ChildList) ->
    tuple_2_binary(Name, T, TomlBin, [{Key, []}|ChildList]);
tuple_2_binary(Name, [{Key, List}|T], TomlBin, ChildList) ->
    {ok, Binary} = utility:join(List, <<",">>),
    NewTomlBin = <<TomlBin/binary, " ", Key/binary, " = [", Binary/binary, "]">>,
    tuple_2_binary(Name, T, NewTomlBin, ChildList);
tuple_2_binary(Name, [], TomlBin, [{Key, List}|T]) when is_list(List) ->
    ChildName = <<Name/binary, ".", Key/binary>>,
    ChildTomlBin = <<TomlBin/binary, " [", ChildName/binary, "]">>,
    {ok, NewTomlBin} = tuple_2_binary(ChildName, List, ChildTomlBin, []),
    tuple_2_binary(Name, [], NewTomlBin, T);
tuple_2_binary(_, [], TomlBin, []) ->
    {ok, TomlBin};
tuple_2_binary(Name, [], TomlBin, ChildList) ->
    NewChildList = lists:reverse(ChildList),
    tuple_2_binary(Name, [], TomlBin, NewChildList).


list_2_binary([H|T]) ->
    {ok, TomlBin} = tuple_2_binary(H),
    list_2_binary(T, TomlBin).
list_2_binary([H|T], TomlBin1) ->
    {ok, TomlBin2} = tuple_2_binary(H),
    NewTomlBin = <<TomlBin1/binary, " ", TomlBin2/binary>>,
    list_2_binary(T, NewTomlBin);
list_2_binary([], TomlBin) ->
    {ok, TomlBin}.


binary_2_term([$[, $[|T], TomlList) ->
    {ok, NameBin, RestStr} = parse_top_name(T),
    {ok, TomlTuple, RestTomlStr} = parse_attrs(NameBin, RestStr),
    binary_2_term(RestTomlStr, [TomlTuple|TomlList]);
binary_2_term([], TomlList) ->
    {ok, TomlList}.


parse_top_name(Str) ->
    parse_top_name(Str, []).
parse_top_name([$], $]|T], Name) ->
    NameBin = erlang:list_to_binary(lists:reverse(Name)),
    {ok, NameBin, T};
parse_top_name([H|T], Name) ->
    parse_top_name(T, [H|Name]).


parse_attrs(NameBin, RestStr) ->
    parse_attrs(NameBin, RestStr, []).
parse_attrs(NameBin, [$[, $[|_] = RestTomlStr, Attrs) ->
    {ok, {NameBin, Attrs}, RestTomlStr};
parse_attrs(NameBin, [], Attrs) ->
    {ok, {NameBin, Attrs}, []};
parse_attrs(NameBin, [$[|_] = RestStr, Attrs) ->
    {ok, NewAttrs, RestTomlStr} = parse_children(NameBin, Attrs, RestStr),
    {ok, {NameBin, NewAttrs}, RestTomlStr};
parse_attrs(NameBin, RestStr, Attrs) ->
    case parse_key(RestStr) of
        {ok, [], []} ->
            {ok, {NameBin, Attrs}, []};
        {ok, Key, RestStr1} ->
            {ok, Value, RestStr2} = parse_value(RestStr1),
            {ok, NewRestStr} = drop_space(RestStr2),
            parse_attrs(NameBin, NewRestStr, [{Key, Value}|Attrs])
    end.


parse_children(Name, Attrs, RestStr) ->
    parse_children(Name, Attrs, RestStr, []).
parse_children(_, Attrs, [], Children) ->
    {ok, NewAttrs} = merge_attr(Attrs, Children),
    {ok, NewAttrs, []};
parse_children(_, Attrs, [$[, $[|_] = RestStr, Children) ->
    {ok, NewAttrs} = merge_attr(Attrs, Children),
    {ok, NewAttrs, RestStr};
parse_children(Name, Attrs, RestStr, Children) ->
    {ok, ChildNameList, RestStr1} = parse_child_name(Name, RestStr),
    {ok, Child, NewRestStr} = parse_child(RestStr1, ChildNameList),
    parse_children(Name, Attrs, NewRestStr, [Child|Children]).


parse_child_name(Name, RestStr) ->
    parse_child_name(Name, RestStr, []).
parse_child_name(NameBin, [$[|T], []) ->
    parse_child_name(NameBin, T, []);
parse_child_name(NameBin, [$]|T], Result) ->
    NameStr = lists:reverse(Result),
    [NameBin|ChildNameList] = re:split(NameStr, "[.]"),
    {ok, lists:reverse(ChildNameList), T};
parse_child_name(NameBin, [H|T], Result) ->
    parse_child_name(NameBin, T, [H|Result]).


parse_child(RestStr, ChildNameList) ->
    parse_child(RestStr, ChildNameList, []).
parse_child([$[|_] = RestStr, [H|T], Attr) ->
    parse_child(RestStr, T, [{H, Attr}]);
parse_child([], [H|T], Attr) ->
    parse_child([], T, [{H, Attr}]);
parse_child(RestStr, [], [Attr]) ->
    {ok, Attr, RestStr};
parse_child(RestStr, [], []) ->
    {ok, [], RestStr};
parse_child(RestStr, ChildNameList, Attr) ->
    {ok, Key, RestStr1} = parse_key(RestStr),
    {ok, Value, RestStr2} = parse_value(RestStr1),
    {ok, NewRestStr} = drop_space(RestStr2),
    parse_child(NewRestStr, ChildNameList, [{Key, Value}|Attr]).


merge_attr(Attrs, [{ChildName, ChildAttrs}|T]) ->
    case lists:keyfind(ChildName, 1, Attrs) of
        false ->
            NewAttrs = [{ChildName, ChildAttrs}|Attrs],
            merge_attr(NewAttrs, T);
        {ChildName, OriginChildAttrs} ->
            {ok, NewChildAttrs} = merge_attr(OriginChildAttrs, ChildAttrs),
            NewAttrs = lists:keystore(ChildName, 1, Attrs, {ChildName, NewChildAttrs}),
            merge_attr(NewAttrs, T)
    end;
merge_attr(Attrs, []) ->
    {ok, Attrs}.


parse_key(RestStr) ->
    parse_key(RestStr, []).
parse_key([$\s|T], Key) ->
    parse_key(T, Key);
parse_key([$\t|T], Key) ->
    parse_key(T, Key);
parse_key([$\r|T], Key) ->
    parse_key(T, Key);
parse_key([$\n|T], Key) ->
    parse_key(T, Key);
parse_key([$=|T], Key) ->
    KeyBin = erlang:list_to_binary(lists:reverse(Key)),
    {ok, KeyBin, T};
parse_key([], []) ->
    {ok, [], []};
parse_key([H|T], Key) ->
    parse_key(T, [H|Key]).


parse_value([$\s|T]) ->
    parse_value(T);
parse_value([$\t|T]) ->
    parse_value(T);
parse_value([$\r|T]) ->
    parse_value(T);
parse_value([$\n|T]) ->
    parse_value(T);
parse_value([$\"|T]) ->
    parse_string_value(T, []);
parse_value([$[|T]) ->
    parse_list_value(T, []);
parse_value(RestStr) ->
    parse_integer_value(RestStr, []).


parse_string_value([$\"|T], Value) ->
    ValueBin = erlang:list_to_binary(lists:reverse(Value)),
    {ok, ValueBin, T};
parse_string_value([H|T], Value) ->
    parse_string_value(T, [H|Value]).


parse_integer_value([$0|T], Value) ->
    parse_integer_value(T, [$0|Value]);
parse_integer_value([$1|T], Value) ->
    parse_integer_value(T, [$1|Value]);
parse_integer_value([$2|T], Value) ->
    parse_integer_value(T, [$2|Value]);
parse_integer_value([$3|T], Value) ->
    parse_integer_value(T, [$3|Value]);
parse_integer_value([$4|T], Value) ->
    parse_integer_value(T, [$4|Value]);
parse_integer_value([$5|T], Value) ->
    parse_integer_value(T, [$5|Value]);
parse_integer_value([$6|T], Value) ->
    parse_integer_value(T, [$6|Value]);
parse_integer_value([$7|T], Value) ->
    parse_integer_value(T, [$7|Value]);
parse_integer_value([$8|T], Value) ->
    parse_integer_value(T, [$8|Value]);
parse_integer_value([$9|T], Value) ->
    parse_integer_value(T, [$9|Value]);
parse_integer_value(RestStr, Value) ->
    ValueInteger = erlang:list_to_integer(lists:reverse(Value)),
    {ok, ValueInteger, RestStr}.


parse_list_value([$\"|T], Value) ->
    {ok, Item, RestStr} = parse_string_value(T, []),
    parse_list_value(RestStr, [Item|Value]);
parse_list_value([$,|T], Value) ->
    parse_list_value(T, Value);
parse_list_value([$\s|T], Value) ->
    parse_list_value(T, Value);
parse_list_value([$\t|T], Value) ->
    parse_list_value(T, Value);
parse_list_value([$\r|T], Value) ->
    parse_list_value(T, Value);
parse_list_value([$\n|T], Value) ->
    parse_list_value(T, Value);
parse_list_value([$]|RestStr], Value) ->
    {ok, Value, RestStr};
parse_list_value([], Value) ->
    {ok, Value, []};
parse_list_value(RestStr, Value) ->
    {ok, Item, NewRestStr} = parse_integer_value(RestStr, []),
    parse_list_value(NewRestStr, [Item|Value]).


drop_space([$\s|T]) ->
    drop_space(T);
drop_space([$\t|T]) ->
    drop_space(T);
drop_space([$\r|T]) ->
    drop_space(T);
drop_space([$\n|T]) ->
    drop_space(T);
drop_space(Rest) ->
    {ok, Rest}.