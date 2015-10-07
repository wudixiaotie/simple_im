%% ===================================================================
%% Author xiaotie
%% 2015-10-6
%% toml helper for simple_im
%% ===================================================================

-module (toml).

-export ([term_2_binary/1, binary_2_term/1]).
-compile (export_all).



%% ===================================================================
%% API functions
%% ===================================================================

term_2_binary(Tuple) when is_tuple(Tuple) ->
    tuple_2_binary(Tuple);
term_2_binary(List) when is_list(List) ->
    list_2_binary(List);
term_2_binary(_) ->
    {error, wrong_type}.


binary_2_term(Binary) ->
    List = erlang:binary_to_list(Binary),
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
tuple_2_binary(Name, [{Key, List}|T], TomlBin, ChildList) when is_list(List) ->
    tuple_2_binary(Name, T, TomlBin, [{Key, List}|ChildList]);
tuple_2_binary(Name, [], TomlBin, [{Key, List}|T]) when is_list(List) ->
    SubName = <<Name/binary, ".", Key/binary>>,
    SubTomlBin = <<TomlBin/binary, " [", SubName/binary, "]">>,
    {ok, NewTomlBin} = tuple_2_binary(SubName, List, SubTomlBin, []),
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
    {ok, NameBin, RestStr} = get_name(T),
    {ok, TomlTuple, RestTomlStr} = get_attrs(NameBin, RestStr),
    binary_2_term(RestTomlStr, [TomlTuple|TomlList]);
binary_2_term([], TomlList) ->
    {ok, TomlList}.


get_name(Str) ->
    get_name(Str, []).
get_name([$], $]|T], Name) ->
    NameBin = erlang:list_to_binary(lists:reverse(Name)),
    {ok, NameBin, T};
get_name([H|T], Name) ->
    get_name(T, [H|Name]).


get_attrs(NameBin, RestStr) ->
    get_attrs(NameBin, RestStr, []).
get_attrs(NameBin, [$[, $[|_] = RestTomlStr, AttrList) ->
    {ok, {NameBin, AttrList}, RestTomlStr};
get_attrs(NameBin, [], AttrList) ->
    {ok, {NameBin, AttrList}, []};
get_attrs(NameBin, [$[|T], AttrList) ->
    {ok, NewAttrList, RestStr} = get_child(NameBin, T, AttrList),
    get_attrs(NameBin, RestStr, NewAttrList);
get_attrs(NameBin, RestStr, AttrList) ->
    {ok, Key, RestStr1} = get_key(RestStr),
    {ok, Value, RestStr2} = get_value(RestStr1),
    {ok, NewRestStr} = drop_space(RestStr2),
    get_attrs(NameBin, NewRestStr, [{Key, Value}|AttrList]).


% get_child(NameBin, RestStr, AttrList) ->
%     {ok, ChildTitles, RestStr1} = get_title(NameBin, RestStr, []),
%     get_child(NameBin, RestStr1, AttrList, ChildTitles).
% get_child(NameBin, RestStr, AttrList, [H|T]) ->
%     case lists:keyfind(H, 1, AttrList) of
%         false ->
%             ,



get_title(NameBin, [$]|T], TitleStr) ->
    TitleStr1 = lists:reverse(TitleStr),
    [NameBin|ChildTitles] = re:split(TitleStr1, "[.]"),
    {ok, ChildTitles, T};
get_title(NameBin, [H|T], TitleStr) ->
    get_title(NameBin, T, [H|TitleStr]).


get_key(RestStr) ->
    get_key(RestStr, []).
get_key([$\s|T], Key) ->
    get_key(T, Key);
get_key([$\t|T], Key) ->
    get_key(T, Key);
get_key([$\r|T], Key) ->
    get_key(T, Key);
get_key([$\n|T], Key) ->
    get_key(T, Key);
get_key([$=|T], Key) ->
    KeyBin = erlang:list_to_binary(lists:reverse(Key)),
    {ok, KeyBin, T};
get_key([H|T], Key) ->
    get_key(T, [H|Key]).


get_value([$\s|T]) ->
    get_value(T);
get_value([$\t|T]) ->
    get_value(T);
get_value([$\r|T]) ->
    get_value(T);
get_value([$\n|T]) ->
    get_value(T);
get_value([$\"|T]) ->
    get_string_value(T, []);
get_value(RestStr) ->
    get_integer_value(RestStr, []).


get_string_value([$\"|T], Value) ->
    ValueBin = erlang:list_to_binary(lists:reverse(Value)),
    {ok, ValueBin, T};
get_string_value([H|T], Value) ->
    get_string_value(T, [H|Value]).


get_integer_value([$0|T], Value) ->
    get_integer_value(T, [$0|Value]);
get_integer_value([$1|T], Value) ->
    get_integer_value(T, [$1|Value]);
get_integer_value([$2|T], Value) ->
    get_integer_value(T, [$2|Value]);
get_integer_value([$3|T], Value) ->
    get_integer_value(T, [$3|Value]);
get_integer_value([$4|T], Value) ->
    get_integer_value(T, [$4|Value]);
get_integer_value([$5|T], Value) ->
    get_integer_value(T, [$5|Value]);
get_integer_value([$6|T], Value) ->
    get_integer_value(T, [$6|Value]);
get_integer_value([$7|T], Value) ->
    get_integer_value(T, [$7|Value]);
get_integer_value([$8|T], Value) ->
    get_integer_value(T, [$8|Value]);
get_integer_value([$9|T], Value) ->
    get_integer_value(T, [$9|Value]);
get_integer_value(RestStr, Value) ->
    ValueBin = erlang:list_to_integer(lists:reverse(Value)),
    {ok, ValueBin, RestStr}.


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


% {<<"a">>, [{<<"b">>, 3}]}
% <<"[[a]] b = 3">>


% {<<"a">>, [{<<"b">>, 3}, {<<"v">>, [{<<"v1">>, <<"v1v">>}, {<<"v2">>, <<"v2v">>}]}]}
% <<"[[a]] b = 3 [a.v] v1 = \"v1v\" v2 = \"v2v\"">>


% {<<"a">>, [{<<"b">>, 3}, {<<"v">>, [{<<"v1">>, [{<<"v1v">>, <<"fycj">>}]}, {<<"v2">>, <<"v2v">>}]}]}
% <<"[[a]] b = 3 [a.v] v2 = \"v2v\" [a.v.v1] v1v = \"fycj\"">>


% [{<<"a">>, [{<<"b">>, 5}]},
%  {<<"a">>, [{<<"b">>, <<"asdf">>}]}]
% <<"[[a]] b = 5 [[a]] b = \"asdf\"">>


% [{<<"a">>, [{<<"b">>, 3},
%             {<<"v">>, [{<<"v1">>, [{<<"v1v">>, <<"fycj">>}]},
%                        {<<"v2">>, <<"v2v">>}]}]},
%  {<<"a">>, [{<<"b">>, <<"asdf">>},
%             {<<"s">>, [{<<"x">>, 5},
%                        {<<"c">>, <<"hehe">>}]}]}]
% <<"[[a]] b = 3 [a.v] v2 = \"v2v\" [a.v.v1] v1v = \"fycj\" [[a]] b = \"asdf\" [a.s] x = 5 c = \"hehe\"">>






% <<"[[a]] x = 1 [a.v] y = 2 [a.v.f] z =3">>
% <<"[[a]] x = 1 [a.b] y = 1 [a.f] z = 1 [a.b.c] m =2">>