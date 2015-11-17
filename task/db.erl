%% ===================================================================
%% Author xiaotie
%% 2015-11-06
%% database tasks
%% ===================================================================

-module (db).

-export ([init/0]).
-compile (export_all).



%% ===================================================================
%% APIs
%% ===================================================================

init() ->
    log:i("=============Database initializing..~n"),
    FunctionPath = "db/postgresql/function/",
    ok = create_tables(),

    case file:list_dir(FunctionPath) of
        {ok, FunctionFileList} ->
            create_functions(FunctionFileList, FunctionPath);
        _ ->
            log:e("Can not open function directory!")
    end,
    log:i("=============Database all set up.~n"),
    ok.



%% ===================================================================
%% gen_server callbacks
%% ===================================================================

create_tables() ->
    ok.


create_functions([H|T], FunctionPath) ->
    ok = execute_sql(FunctionPath ++ H),
    create_functions(T, FunctionPath);
create_functions([], _) ->
    ok.


execute_sql(SqlFilePath) ->
    log:i("execute sql file: ~p~n", [SqlFilePath]),
    {ok, SqlFileHandler} = file:open(SqlFilePath, [read, binary]),
    {ok, SQL} = scan_file(SqlFileHandler, <<>>),
    {ok, [], []} = postgresql:exec(SQL),
    ok.


scan_file(FileHandler, Result) ->
    case file:read(FileHandler, 1024) of
        eof  ->
            {ok, Result};
        {ok, Binary} ->
            NewResult = <<Result/binary, Binary/binary>>,
            scan_file(FileHandler, NewResult)
    end.