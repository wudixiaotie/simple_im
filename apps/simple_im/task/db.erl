%% ===================================================================
%% Author xiaotie
%% 2015-11-06
%% database tasks
%% ===================================================================

-module(db).

-export([init/0]).



%% ===================================================================
%% APIs
%% ===================================================================

init() ->
    log:i("=============Database initializing..~n"),
    ok = init_table(),
    ok = init_function(),
    ok = init_data(),
    log:i("=============Database all set up.~n"),
    ok.



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init_table() ->
    log:i("=============Tables initializing..~n"),
    Tables = ["users.sql", "groups.sql", "contacts.sql", "group_members.sql",
              "pre_contacts.sql"],
    Path = "doc/db/postgresql/table/",
    TablePathList = lists:map(fun(I) -> Path ++ I end, Tables),
    execute_sql(TablePathList).


init_function() ->
    log:i("=============Functions initializing..~n"),
    SqlFileList = filelib:fold_files("doc/db/postgresql/function",
                                     ".*.sql",
                                     true,
                                     fun(F, AccIn) -> [F | AccIn] end,
                                     []),
    execute_sql(SqlFileList).


execute_sql([SqlFilePath|T]) ->
    log:i("execute sql file: ~p~n", [SqlFilePath]),
    {ok, SqlFileHandler} = file:open(SqlFilePath, [read]),
    ok = parse_sql(SqlFileHandler, []),
    execute_sql(T);
execute_sql([]) ->
    ok.


parse_sql(SqlFileHandler, SQL) ->
    case file:read_line(SqlFileHandler) of
        eof ->
            {ok, [], []} = postgresql:exec(SQL),
            ok;
        {ok, Data} ->
            case utility:strip_head(Data) of
                {ok, [$C, $R, $E, $A, $T, $E|_] = Statement} ->
                    case SQL =/= [] of
                        true ->
                            {ok, [], []} = postgresql:exec(SQL);
                        _ ->
                            ok
                    end,
                    parse_sql(SqlFileHandler, Statement);
                {ok, Statement} ->
                    parse_sql(SqlFileHandler, SQL ++ Statement)
            end
    end.


init_data() ->
    users:create(<<"test1">>, <<"8618266175357">>, <<"888888">>),
    users:create(<<"test2">>, <<"8618501260693">>, <<"888888">>),
    users:create(<<"test3">>, <<"8618266175356">>, <<"888888">>).