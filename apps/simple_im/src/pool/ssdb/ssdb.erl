%% ===================================================================
%% Author xiaotie
%% 2016-05-14
%% ssdb interface
%% ===================================================================

-module(ssdb).

-export([start_link/0, q/1]).


%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    case erlang:whereis(ssdb_client_sup) of
        undefined ->
            {ok, Pid} = ssdb_client_sup:start_link(),
            PoolSize = env:get(ssdb_poolsize),
            ok = start_client(PoolSize);
        Pid ->
            ok
    end,
    {ok, Pid}.


q(Request) ->
    Packet = pack(Request),
    PoolSize = env:get(ssdb_poolsize),
    {ok, Index} = utility:random_number(PoolSize),
    Name = client_name(Index),
    Reply = gen_server:call(Name, {q, Packet}),
    unpack(Reply).



%% ===================================================================
%% Internal functions
%% ===================================================================

start_client(0) ->
    ok;
start_client(PoolSize) ->
    Name = client_name(PoolSize),
    {ok, _} = supervisor:start_child(ssdb_client_sup, [Name]),
    start_client(PoolSize - 1).


client_name(Index) ->
    erlang:list_to_atom("ssdb_client_" ++ erlang:integer_to_list(Index)).


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