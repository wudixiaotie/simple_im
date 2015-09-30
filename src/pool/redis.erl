-module (redis).

-export ([start_link/0, q/1, key/1]).



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
    <<"client_", Token/binary>>;
key(im_list) ->
    <<"im_list">>.