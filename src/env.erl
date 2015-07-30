-module (env).

-export ([get/1]).

-define (DEFAULT_PORT, 1987).
% 10 minutes
-define (DEFAULT_HEARTBEAT_TIMEOUT, 6000).
% -define (DEFAULT_HEARTBEAT_TIMEOUT, 600000).

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
get_default(heartbeat_timeout) -> ?DEFAULT_HEARTBEAT_TIMEOUT.