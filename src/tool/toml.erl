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

term_2_binary(Term) when is_tuple(Term) ->
    ok.


binary_2_term(Binary) ->
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================

tuple_2_binary({Name, Attrs}) ->
    tuple_to_toml()

tuple_to_toml({Name, Attrs}) ->
    tuple_to_toml(none, {Name, Attrs}).

tuple_to_toml(none, {Name, Attrs}) ->
    tuple_to_toml(Name, Attrs, [], <<"[", Name/binary, "]">>);
tuple_to_toml(FatherName, {Name, Attrs}) ->
    tuple_to_toml(Name, Attrs, [], <<"[", FatherName/binary, ".", Name/binary, "]">>).

tuple_to_toml(Name, [{Key, Value}|T], ChildBinList, Result) when is_list(Value) ->
    {ok, ChildBin} = tuple_to_toml(Name, {Key, Value}),
    tuple_to_toml(Name, T, [ChildBin|ChildBinList], Result);
tuple_to_toml(Name, [H|T], ChildBinList, Result) ->
    {ok, Bin} = key_value_to_toml(H),
    NewResult = <<Result/binary, " ", Bin/binary>>,
    tuple_to_toml(Name, T, ChildBinList, NewResult);
tuple_to_toml(Name, [], [H|T], Result) ->
    NewResult = <<Result/binary, " ", H/binary>>,
    tuple_to_toml(Name, [], T, NewResult);
tuple_to_toml(_, _, _, Result) ->
    {ok, Result}.



% { \"fruit\": [
%     {
%       \"name\": \"apple\",
%       \"physical\": {
%         \"color\": \"red\",
%         \"shape\": \"round\"
%       },
%       \"variety\": [
%         { \"name\": \"red delicious\" },
%         { \"name\": \"granny smith\" }
%       ]
%     },
%     {
%       \"name\": \"banana\",
%       \"variety\": [
%         { \"name\": \"plantain\" }
%       ]
%     }
%   ]
% }


% [{<<"fruit">>,
%   [[{<<"name">>,<<"apple">>},
%     {<<"physical">>,
%      [{<<"color">>,<<"red">>},{<<"shape">>,<<"round">>}]},
%     {<<"variety">>,
%      [[{<<"name">>,<<"red delicious">>}],
%       [{<<"name">>,<<"granny smith">>}]]}],
%    [{<<"name">>,<<"banana">>},
%     {<<"variety">>,[[{<<"name">>,<<"plantain">>}]]}]]}]