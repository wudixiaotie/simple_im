-module (env).

-export ([get/1]).

-define (DEFAULT_PORT, 1987).

% @spec get(Key) -> Value
get(Key) ->
    case application:get_env(simple_im, Key) of
        {ok, Value} -> Value;
        undefined -> get_default(Key)
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================
get_default(port) -> ?DEFAULT_PORT.