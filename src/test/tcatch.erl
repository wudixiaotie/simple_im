-module (tcatch).

-compile (export_all).

start() ->
    ets:new(tcatch, [named_table, public]).

ti1() ->
    catch ets:insert(tcatch, {1, 2}).

td1() ->
    catch ets:delete(tcatch, 1).

ti2() ->
    ets:insert(tcatch, {1, 2}).

td2() ->
    ets:delete(tcatch, 1).