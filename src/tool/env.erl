%% ===================================================================
%% Author xiaotie
%% 2015-7-29
%% environment variables get
%% ===================================================================

-module (env).

-export ([get/1]).

-define (DEFAULT_PORT, 1987).

-define (DEFAULT_HTTP_PORT, 8080).

% 10 minutes
-define (DEFAULT_HEARTBEAT_TIMEOUT, 600000).

-define (DEFAULT_NODE_LIST, ['s1@simple_im.com',
                             's2@simple_im.com',
                             's3@simple_im.com']).

-define (DEFAULT_DB_HOST, "localhost").

-define (DEFAULT_DB_USERNAME, "postgres").

-define (DEFAULT_DB_PASSWORD, "postgres").

-define (DEFAULT_DB_DATABASE, "test").

-define (DEFAULT_DB_POOLSIZE, 10).

-define (DEFAULT_REDIS_POOLS, [{pool0, [{size, 10},
                                        {max_overflow, 20},
                                        {host, "127.0.0.1"},
                                        {port, 6379}]}]).

-define (DEFAULT_REDIS_GLOBAL_OR_LOCAL, local).

% @spec get(Key) -> Value
get(Key) ->
    case application:get_env(simple_im, Key) of
        {ok, Value} -> Value;
        undefined -> get_default(Key)
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

get_default(port) -> ?DEFAULT_PORT;
get_default(http_port) -> ?DEFAULT_HTTP_PORT;
get_default(heartbeat_timeout) -> ?DEFAULT_HEARTBEAT_TIMEOUT;
get_default(node_list) -> ?DEFAULT_NODE_LIST;

% database
get_default(db_host) -> ?DEFAULT_DB_HOST;
get_default(db_username) -> ?DEFAULT_DB_USERNAME;
get_default(db_password) -> ?DEFAULT_DB_PASSWORD;
get_default(db_database) -> ?DEFAULT_DB_DATABASE;
get_default(db_poolsize) -> ?DEFAULT_DB_POOLSIZE;

% redis
get_default(redis_pools) -> ?DEFAULT_REDIS_POOLS;
get_default(redis_global_or_local) -> ?DEFAULT_REDIS_GLOBAL_OR_LOCAL.