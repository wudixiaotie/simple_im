%% ===================================================================
%% Author xiaotie
%% 2015-7-29
%% environment variables get
%% ===================================================================

-module(env).

-export([get/1, set/2]).

-include("simple_im.hrl").

-define(DEFAULT_APP_MODE, im).

-define(DEFAULT_LOG_DIR, "/tmp/log/").

-define(DEFAULT_CACERTFILE, "cowboy-ca.crt").

-define(DEFAULT_CERTFILE, "server.crt").

-define(DEFAULT_KEYFILE, "server.key").

-define(DEFAULT_IM_LISTENER_POOLSIZE, 2).

-define(DEFAULT_IM_PORT, 1987).

-define(DEFAULT_HTTP_PORT, 8080).

-define(DEFAULT_MIDDLEMAN_HOST, "localhost").

-define(DEFAULT_MIDDLEMAN_PORT, 10000).

-define(DEFAULT_SESSION_FILE, "/tmp/session.dets").

-define(DEFAULT_CLIENT_FACTORY_SIZE, 10).

% 10 minutes
-define(DEFAULT_HEARTBEAT_TIMEOUT, 600000).

-define(DEFAULT_INITIAL_NODE, 's1@simple_im.com').

-define(DEFAULT_SESSION_SERVER_NODE, 'session_server@simple_im.com').

-define(DEFAULT_DB_HOST, "localhost").

-define(DEFAULT_DB_USERNAME, "postgres").

-define(DEFAULT_DB_PASSWORD, "postgres").

-define(DEFAULT_DB_DATABASE, "test").

-define(DEFAULT_DB_PORT, 5432).

-define(DEFAULT_DB_POOLSIZE, 5).

-define(DEFAULT_REDIS_POOLS, [{pool0, [{size, 10},
                                       {max_overflow, 20},
                                       {host, "127.0.0.1"},
                                       {port, 6379}]}]).

-define(DEFAULT_SSDB, [{host, "127.0.0.1"}, {port, 8888}]).

-define(DEFAULT_SSDB_POOLSIZE, 30).

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
    case application:get_env(?APPLICATION_NAME, Key) of
        {ok, Value} -> Value;
        _ ->
            Value = get_default(Key),
            ok = set(Key, Value),
            Value
    end.


% @spec set(Key, Value) -> ok
set(Key, Value) ->
    ok = application:set_env(?APPLICATION_NAME, Key, Value),
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================

get_default(app_mode) -> ?DEFAULT_APP_MODE;
get_default(log_dir) -> ?DEFAULT_LOG_DIR;
get_default(cacertfile) -> ?DEFAULT_CACERTFILE;
get_default(certfile) -> ?DEFAULT_CERTFILE;
get_default(keyfile) -> ?DEFAULT_KEYFILE;
get_default(im_listener_poolsize) -> ?DEFAULT_IM_LISTENER_POOLSIZE;
get_default(im_port) -> ?DEFAULT_IM_PORT;
get_default(http_port) -> ?DEFAULT_HTTP_PORT;
get_default(middleman_host) -> ?DEFAULT_MIDDLEMAN_HOST;
get_default(middleman_port) -> ?DEFAULT_MIDDLEMAN_PORT;
get_default(session_file) -> ?DEFAULT_SESSION_FILE;
get_default(client_factory_size) -> ?DEFAULT_CLIENT_FACTORY_SIZE;
get_default(heartbeat_timeout) -> ?DEFAULT_HEARTBEAT_TIMEOUT;
get_default(initial_node) -> ?DEFAULT_INITIAL_NODE;
get_default(session_server_node) -> ?DEFAULT_SESSION_SERVER_NODE;

% database
get_default(db_host) -> ?DEFAULT_DB_HOST;
get_default(db_username) -> ?DEFAULT_DB_USERNAME;
get_default(db_password) -> ?DEFAULT_DB_PASSWORD;
get_default(db_database) -> ?DEFAULT_DB_DATABASE;
get_default(db_port) -> ?DEFAULT_DB_PORT;
get_default(db_poolsize) -> ?DEFAULT_DB_POOLSIZE;

% redis
get_default(redis_pools) -> ?DEFAULT_REDIS_POOLS;
get_default(redis_global_or_local) -> ?DEFAULT_REDIS_GLOBAL_OR_LOCAL;

% ssdb
get_default(ssdb) -> ?DEFAULT_SSDB;
get_default(ssdb_poolsize) -> ?DEFAULT_SSDB_POOLSIZE;

get_default(device_list) -> ?DEFAULT_DEVICE_LIST;

% http server config
get_default(audio_dir) -> ?DEFAULT_AUDIO_DIR;
get_default(video_dir) -> ?DEFAULT_VIDEO_DIR;
get_default(image_dir) -> ?DEFAULT_IMAGE_DIR;
get_default(_) -> undefined.