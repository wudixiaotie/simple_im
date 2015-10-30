%% ===================================================================
%% Author xiaotie
%% 2015-9-12
%% utility for simple im
%% ===================================================================

-module (utility).

-export ([md5_hex_32/1, random_binary_16/0, random_number/1,
          guid/0, free_port/1, index_of/2, ip_port/2,
          timestamp/0, delete_from_list/2, join/2]).



%% ===================================================================
%% APIs
%% ===================================================================

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
    RandomBytes = crypto:strong_rand_bytes(8),
    TimestampBytes = erlang:md5(erlang:term_to_binary(os:timestamp())),
    {ok, base64:encode(<<RandomBytes/binary, TimestampBytes/binary>>)}.


free_port(Port) when Port < 65536 ->
    case gen_tcp:connect("localhost", Port, []) of
        {error, _Reason} ->
            {ok, Port};
        {ok, Socket} ->
            gen_tcp:close(Socket),
            free_port(Port + 1)
    end;
free_port(_) ->
    {error, out_of_range}.


index_of(List, Item) ->
    index_of(List, Item, 0).


ip_port(IP, Port) when is_tuple(IP) ->
    ip_port(inet_parse:ntoa(IP), Port);
ip_port(IP, Port) when is_list(IP) ->
    ip_port(erlang:list_to_binary(IP), Port);
ip_port(IP, Port) when is_integer(Port) ->
    ip_port(IP, erlang:integer_to_binary(Port));
ip_port(IP, Port) when is_list(IP) ->
    ip_port(IP, erlang:list_to_binary(Port));
ip_port(IP, Port) ->
    <<IP/binary, ":", Port/binary>>.


timestamp() ->
    {A, B, _} = os:timestamp(),
    A * 1000000 + B.


delete_from_list(Element, List) ->
    delete_from_list(Element, List, []).


join(List, Separator) ->
    join(List, Separator, <<>>).



%% ===================================================================
%% Internal functions
%% ===================================================================

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


index_of([H|_], Item, Index) when H == Item ->
    {ok, Index};
index_of([_|T], Item, Index) ->
    index_of(T, Item, Index + 1);
index_of([], _, _) ->
    {ok, -1}.


delete_from_list(Element, [Element|T], Result) ->
    delete_from_list(Element, T, Result);
delete_from_list(Element, [H|T], Result) ->
    delete_from_list(Element, T, [H|Result]);
delete_from_list(_, [], Result) ->
    {ok, lists:reverse(Result)}.


join([H|[]], _, Result) when is_integer(H) ->
    Bin = erlang:integer_to_binary(H),
    NewResult = <<Result/binary, " ", Bin/binary, " ">>,
    {ok, NewResult};
join([H|[]], _, Result) when is_binary(H) ->
    NewResult = <<Result/binary, " \"", H/binary, " \" ">>,
    {ok, NewResult};
join([H|[]], _, Result) when is_list(H) ->
    Bin = erlang:list_to_binary(H),
    NewResult = <<Result/binary, " \"", Bin/binary, " \" ">>,
    {ok, NewResult};
join([H|[]], _, Result) when is_atom(H) ->
    Bin = erlang:atom_to_binary(H, utf8),
    NewResult = <<Result/binary, " \"", Bin/binary, " \" ">>,
    {ok, NewResult};
join([H|T], Separator, Result) when is_integer(H) ->
    Bin = erlang:integer_to_binary(H),
    NewResult = <<Result/binary, " ", Bin/binary, Separator/binary>>,
    join(T, Separator, NewResult);
join([H|T], Separator, Result) when is_binary(H) ->
    NewResult = <<Result/binary, " \"", H/binary, " \"", Separator/binary>>,
    join(T, Separator, NewResult);
join([H|T], Separator, Result) when is_list(H) ->
    Bin = erlang:list_to_binary(H),
    NewResult = <<Result/binary, " \"", Bin/binary, " \"", Separator/binary>>,
    join(T, Separator, NewResult);
join([H|T], Separator, Result) when is_atom(H) ->
    Bin = erlang:atom_to_binary(H, utf8),
    NewResult = <<Result/binary, " \"", Bin/binary, " \"", Separator/binary>>,
    join(T, Separator, NewResult).