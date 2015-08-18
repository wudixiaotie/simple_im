-module (test_mnesia_split).

-export ([start/0, dw/0, tdw/0]).

-record (test_mnesia, {userid, pid}).

start() ->
    mnesia:stop(),
    mnesia:delete_schema([nodes()]),
    mnesia:create_schema([nodes()]),
    mnesia:start(),
    {atomic,ok} = mnesia:create_table(test_mnesia, [{ram_copies, [nodes()]},
                                                    {attributes, record_info(fields, test_mnesia)}]),
    {atomic,ok} = mnesia:add_table_index(test_mnesia, pid),
    {ok, []}.

dw() ->
    % UserId = <<"user_", (integer_to_binary(1))/binary, "@android">>,
    UserId = <<"user_1@android">>,
    mnesia:dirty_write(#test_mnesia{userid = UserId, pid = UserId}).

tdw() ->
    tc:ct(?MODULE, dw, [], 100000).


%% ===================================================================
%% Internal functions
%% ===================================================================