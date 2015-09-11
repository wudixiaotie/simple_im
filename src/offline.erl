-module (offline).

-export ([store/2, get/1]).

% you can use redis ssdb mongodb mysql postgresql as your offline message db.

store(UserId, Msg) ->

    ok.

get(UserId) ->
    {ok, []}.