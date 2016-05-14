%% ===================================================================
%% Author xiaotie
%% 2016-05-14
%% ssdb interface
%% ===================================================================

-module(ssdb).

-export([start_link/0, q/1]).
-compile (export_all).


%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    ssdb_client:start_link().


q(Request) ->
    Packet = pack(Request),
    Reply = gen_server:call(ssdb_client, {q, Packet}),
    unpack(Reply).



%% ===================================================================
%% Internal functions
%% ===================================================================

pack(Request) when is_list(Request), length(Request) > 0 ->
    pack(Request, <<>>);
pack(_) ->
    erlang:error("ssdb command invalid!").
pack([H|T], Packet) ->
    Len = erlang:integer_to_binary(erlang:size(H)),
    NewPacket = <<Packet/binary, Len/binary, "\n", H/binary, "\n">>,
    pack(T, NewPacket);
pack([], Packet) ->
    <<Packet/binary, "\n">>.


unpack(Binary) ->
    unpack(Binary, []).
unpack(Rest, Result) ->
    {match, [{DataLenSize, _}]} = re:run(Rest, "\n"),
    case DataLenSize of
        0 ->
            lists:reverse(Result);
        _ ->
            <<DataLenBin:DataLenSize/binary, "\n", Rest1/binary>> = Rest,
            DataLen = erlang:binary_to_integer(DataLenBin),
            <<DataBin:DataLen/binary, "\n", Rest2/binary>> = Rest1,
            unpack(Rest2, [DataBin|Result])
    end.