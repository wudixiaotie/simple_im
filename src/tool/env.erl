-module (env).

-export ([get/1]).

-define (DEFAULT_PORT, 1987).
% 10 minutes
-define (DEFAULT_HEARTBEAT_TIMEOUT, 600000).

-define (DEFAULT_FATHER_NODE, 's1@192.168.1.137').

-define (DEFAULT_DB_HOST, "localhost").

-define (DEFAULT_DB_USERNAME, "postgres").

-define (DEFAULT_DB_PASSWORD, "postgres").

-define (DEFAULT_DB_DATABASE, "test").

-define (DEFAULT_DB_POOLSIZE, 10).

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
get_default(heartbeat_timeout) -> ?DEFAULT_HEARTBEAT_TIMEOUT;
get_default(father_node) -> ?DEFAULT_FATHER_NODE;

% database
get_default(db_host) -> ?DEFAULT_DB_HOST;
get_default(db_username) -> ?DEFAULT_DB_USERNAME;
get_default(db_password) -> ?DEFAULT_DB_PASSWORD;
get_default(db_database) -> ?DEFAULT_DB_DATABASE;
get_default(db_poolsize) -> ?DEFAULT_DB_POOLSIZE.