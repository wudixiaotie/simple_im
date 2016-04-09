-module (tm1).

-compile (export_all).

-record(session, {sid, usr, us, priority, info}).

create_table() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    {atomic,ok} = mnesia:create_table(session, [{ram_copies, [node()]},
                                                {attributes, record_info(fields, session)}]),
    mnesia:add_table_index(session, usr),
    mnesia:add_table_index(session, us),
    mnesia:stop().

start() ->
    create_table(),
    reset_table(10000).

reset_table(N) ->
    mnesia:start(),
    mnesia:clear_table(session),
    generate_data(N).

generate_data(N) when N > 0 ->
    BN = integer_to_binary(N),
    S = #session{sid = {{1438,65258,N},self()},
                 usr = {BN,<<"server.camwowo.com">>,<<"android">>},
                 us = {BN,<<"server.camwowo.com">>},
                 priority = 0,
                 info = [{ip,{{192,168,1,137},38784}},{conn,c2s},{auth_module,unknown}]},
    mnesia:dirty_write(S),
    generate_data(N - 1);
generate_data(_) -> ok.

table_size() ->
    mnesia:table_info(session,size).

find() ->
    mnesia:dirty_index_read(session, {<<"20844">>,<<"server.camwowo.com">>}, #session.us).