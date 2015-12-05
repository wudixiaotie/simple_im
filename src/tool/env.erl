%% ===================================================================
%% Author xiaotie
%% 2015-7-29
%% environment variables get
%% ===================================================================

-module(env).

-export([get/1]).

-define(CACERTFILE, "cowboy-ca.crt").

-define(CERTFILE, "server.crt").

-define(KEYFILE, "server.key").

-define(DEFAULT_IM_LISTENER_POOLSIZE, 3).

-define(DEFAULT_IM_PORT, 1987).

-define(DEFAULT_HTTP_PORT, 8080).

-define(DEFAULT_MIDDLEMAN_HOST, "localhost").

-define(DEFAULT_MIDDLEMAN_PORT, 10000).

-define(DEFAULT_CF_SIZE, 10).

% 10 minutes
-define(DEFAULT_HEARTBEAT_TIMEOUT, 600000).

-define(DEFAULT_NODE_LIST, ['s1@simple_im.com',
                             's2@simple_im.com',
                             's3@simple_im.com']).

-define(DEFAULT_DB_HOST, "localhost").

-define(DEFAULT_DB_USERNAME, "postgres").

-define(DEFAULT_DB_PASSWORD, "postgres").

-define(DEFAULT_DB_DATABASE, "test").

-define(DEFAULT_DB_PORT, 5432).

-define(DEFAULT_DB_POOLSIZE, 5).

-define(DEFAULT_HTTP_DB_POOLSIZE, 10).

-define(DEFAULT_REDIS_POOLS, [{pool0, [{size, 10},
                                       {max_overflow, 20},
                                       {host, "127.0.0.1"},
                                       {port, 6379}]}]).

-define(DEFAULT_REDIS_GLOBAL_OR_LOCAL, local).

-define(DEFAULT_DEVICE_LIST, [<<"android">>, <<"iphone">>, <<"ipad">>]).

-define(DEFAULT_AUDIO_DIR, "/tmp/file/audio/").

-define(DEFAULT_VIDEO_DIR, "/tmp/file/video/").

-define(DEFAULT_IMAGE_DIR, "/tmp/file/image/").



%% ===================================================================
%% API functions
%% ===================================================================

% @spec get(Key) -> Value
get(Key) ->
    case application:get_env(simple_im, Key) of
        {ok, Value} -> Value;
        undefined -> get_default(Key)
    end.



%% ===================================================================
%% Internal functions
%% ===================================================================

get_default(cacertfile) -> ?CACERTFILE;
get_default(certfile) -> ?CERTFILE;
get_default(keyfile) -> ?KEYFILE;
get_default(im_listener_poolsize) -> ?DEFAULT_IM_LISTENER_POOLSIZE;
get_default(im_port) -> ?DEFAULT_IM_PORT;
get_default(http_port) -> ?DEFAULT_HTTP_PORT;
get_default(middleman_host) -> ?DEFAULT_MIDDLEMAN_HOST;
get_default(middleman_port) -> ?DEFAULT_MIDDLEMAN_PORT;
get_default(cf_size) -> ?DEFAULT_CF_SIZE;
get_default(heartbeat_timeout) -> ?DEFAULT_HEARTBEAT_TIMEOUT;
get_default(node_list) -> ?DEFAULT_NODE_LIST;

% database
get_default(db_host) -> ?DEFAULT_DB_HOST;
get_default(db_username) -> ?DEFAULT_DB_USERNAME;
get_default(db_password) -> ?DEFAULT_DB_PASSWORD;
get_default(db_database) -> ?DEFAULT_DB_DATABASE;
get_default(db_port) -> ?DEFAULT_DB_PORT;
get_default(db_poolsize) -> ?DEFAULT_DB_POOLSIZE;
get_default(http_db_poolsize) -> ?DEFAULT_HTTP_DB_POOLSIZE;

% redis
get_default(redis_pools) -> ?DEFAULT_REDIS_POOLS;
get_default(redis_global_or_local) -> ?DEFAULT_REDIS_GLOBAL_OR_LOCAL;

get_default(device_list) -> ?DEFAULT_DEVICE_LIST;
get_default(audio_dir) -> ?DEFAULT_AUDIO_DIR;
get_default(video_dir) -> ?DEFAULT_VIDEO_DIR;
get_default(image_dir) -> ?DEFAULT_IMAGE_DIR.