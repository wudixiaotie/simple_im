%% ===================================================================
%% Author xiaotie
%% 2015-10-26
%% pre_contacts
%% ===================================================================

-module(pre_contacts).

-export([create/3, find/1]).



%% ===================================================================
%% APIs
%% ===================================================================

create(AUserId, BUserId, Message)
    when is_integer(AUserId), is_integer(BUserId) ->
    SQL = <<"SELECT create_pre_contact($1, $2, $3);">>,
    {ok, _, [{Result}]} = postgresql:exec(SQL, [AUserId, BUserId, Message]),

    Timestamp = utility:timestamp(),
    TimestampBin = erlang:integer_to_binary(Timestamp),
    AUserIdBin = erlang:integer_to_binary(AUserId),
    BUserIdBin = erlang:integer_to_binary(BUserId),
    ssdb:q([<<"zset">>, <<"pre_contacts_", BUserIdBin/binary>>, <<AUserIdBin/binary, ":", Message>>, TimestampBin]),
    {ok, Result}.


find(UserId) ->
    UserIdBin = erlang:integer_to_binary(UserId),
    case ssdb:q([<<"zrscan">>, <<"pre_contacts_", UserIdBin/binary>>, <<>>, <<>>, <<>>, <<"100000000">>]) of
        [<<"ok">>|SSDBResult] ->
            {ok, Result} = unpack_ssdb(SSDBResult),
            {ok, Result};
        _ ->
            log:e("[SSDB] pre_contacts: find id error id:~p!~n", [UserId]),
            SQL = <<"SELECT a_id, message, updated_at FROM pre_contacts WHERE b_id = $1 order by updated_at desc;">>,
            {ok, _, MySQLResult} = postgresql:exec(SQL, [UserId]),
            {ok, Result} = unpack_mysql(MySQLResult),
            {ok, Result}
    end.



%% ===================================================================
%% Internal functions
%% ===================================================================

unpack_ssdb(SSDBResult) ->
    unpack_ssdb(SSDBResult, []).
unpack_ssdb([], Result) ->
    {ok, Result};
unpack_ssdb([Key, Score|T], Result) ->
    
    unpack_ssdb(T, NewResult);