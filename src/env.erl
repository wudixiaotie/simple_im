-module (env).

-export ([get/1]).

-define (DEFAULT_PORT, 1987).
% 10 minutes
-define (DEFAULT_HEARTBEAT_TIMEOUT, 600000).

-define (DEFAULT_FATHER_NODE, 's1@192.168.1.137').

-define (DEFAULT_SESSION_POOLSIZE, 10).

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
get_default(session_poolsize) -> ?DEFAULT_SESSION_POOLSIZE.