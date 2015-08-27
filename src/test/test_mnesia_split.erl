-module (test_mnesia_split).

-export ([start/0, dw/0, tdw/0, stop/0, dw1/0, tdw1/0]).

-record (test_mnesia, {userid, pid}).
-record (test_mnesia1, {userid, pid}).
-record (test_mnesia2, {userid, pid}).
-record (test_mnesia3, {userid, pid}).

start() ->
    NodeList = node_list(),
    rpc_call(mnesia, stop, [], stopped),
    mnesia:delete_schema(NodeList),
    mnesia:create_schema(NodeList),
    rpc_call(mnesia, start, [], ok),
    {atomic,ok} = mnesia:create_table(test_mnesia, [{ram_copies, NodeList},
                                                    {attributes, record_info(fields, test_mnesia)}]),
    {atomic,ok} = mnesia:create_table(test_mnesia1, [{ram_copies, NodeList},
                                                    {attributes, record_info(fields, test_mnesia1)}]),
    {atomic,ok} = mnesia:create_table(test_mnesia2, [{ram_copies, NodeList},
                                                    {attributes, record_info(fields, test_mnesia2)}]),
    {atomic,ok} = mnesia:create_table(test_mnesia3, [{ram_copies, NodeList},
                                                    {attributes, record_info(fields, test_mnesia3)}]),
    {atomic,ok} = mnesia:add_table_index(test_mnesia, pid).
    % rpc_call(mnesia, stop, [], stopped).

stop() ->
    NodeList = node_list(),
    rpc_call(mnesia, stop, [], stopped),
    mnesia:delete_schema(NodeList).


dw() ->
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    random:seed (A, B, C),
    Random = random:uniform(3),
    UserId = <<"user_1@android">>,
    mnesia:dirty_write(#test_mnesia{userid = UserId, pid = UserId}).

tdw() ->
    tc:ct(?MODULE, dw, [], 100000).

dw1() ->
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    random:seed (A, B, C),
    Random = random:uniform(3),
    UserId = <<"user_1@android">>,
    case Random of
        1 ->
            mnesia:dirty_write(#test_mnesia1{userid = UserId, pid = UserId});
        2 ->
            mnesia:dirty_write(#test_mnesia2{userid = UserId, pid = UserId});
        3 ->
            mnesia:dirty_write(#test_mnesia3{userid = UserId, pid = UserId});
        _ ->
            mnesia:dirty_write(#test_mnesia{userid = UserId, pid = UserId})
    end.

tdw1() ->
    tc:ct(?MODULE, dw1, [], 100000).



%% ===================================================================
%% Internal functions
%% ===================================================================
node_list() ->
    [node() | nodes()].

rpc_call(Module, Function, Args, Result) ->
    rpc_call(node_list(), Module, Function, Args, Result).
rpc_call([H|T], Module, Function, Args, Result) ->
    Result = rpc:call(H, Module, Function, Args),
    rpc_call(T, Module, Function, Args, Result);
rpc_call([], _, _, _, _) ->
    ok.













% 3 nodes 3 table
% =====================
% spawn [100000] processes of {test_mnesia_split, dw1, []}:
% Maximum: 11815(μs)      0.011815(s)
% Minimum: 14(μs) 1.4e-5(s)
% Sum: 5492155(μs)        5.492155(s)
% Average: 54.92155(μs)   5.492155e-5(s)
% Greater: 11735
% Less: 88265
% =====================



% 3 nodes 4 table
% =====================
% spawn [100000] processes of {test_mnesia_split, dw1, []}:
% Maximum: 7319(μs)       0.007319(s)
% Minimum: 14(μs) 1.4e-5(s)
% Sum: 2848810(μs)        2.84881(s)
% Average: 28.4881(μs)    2.84881e-5(s)
% Greater: 4219
% Less: 95781
% =====================
