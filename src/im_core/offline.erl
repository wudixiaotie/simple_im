%% ===================================================================
%% Author xiaotie
%% 2015-9-2
%% offline
%% ===================================================================

-module (offline).

-export ([store/2, get/1]).

% offline message expired after 7 days
-define (OFFLINE_EXPIRATION_TIME, "604800").



%% ===================================================================
%% APIs
%% ===================================================================

store(UserId, Msg) ->
    {ok, Key} = make_key(UserId),
    {ok, _} = redis:q([<<"RPUSH">>, Key, Msg]),
    ok.

get(UserId) ->
    {ok, Key} = make_key(UserId),
    {ok, Result} = redis:q([<<"LRANGE">>, Key, <<"0">>, <<"-1">>]),
    {ok, <<"OK">>} = redis:q([<<"LTRIM">>, Key, <<"1">>, <<"0">>]),
    {ok, Result}.



%% ===================================================================
%% Internal functions
%% ===================================================================

make_key(UserId) ->
    UserIdBin = integer_to_binary(UserId),
    Key = <<"offline_", UserIdBin/binary>>,
    {ok, Key}.