%% ===================================================================
%% Author xiaotie
%% 2015-9-12
%% redis interface
%% ===================================================================

-module(redis).

-export([start_link/0, q/1, key/1]).



%% ===================================================================
%% APIs
%% ===================================================================

start_link() ->
    RedisPools = env:get(redis_pools),
    RedisGlobalOrLocal = env:get(redis_global_or_local),
    application:set_env(sharded_eredis, pools, RedisPools),
    application:set_env(sharded_eredis, global_or_local, RedisGlobalOrLocal),
    sharded_eredis_sup:start_link().


q(Request) ->
    sharded_eredis:q(Request).


key({token, Token}) ->
    Key = <<"client_", Token/binary>>,
    {ok, Key};
key({offline, UserId}) ->
    UserIdBin = integer_to_binary(UserId),
    Key = <<"offline_", UserIdBin/binary>>,
    {ok, Key};
key({offline_msg, UserId, MsgId}) ->
    UserIdBin = integer_to_binary(UserId),
    Key = <<"offline_", UserIdBin/binary, "_", MsgId/binary>>,
    {ok, Key};
key(im_list) ->
    Key = <<"im_list">>,
    {ok, Key}.