%%%-------------------------------------------------------------------
%% @doc simple_im public API
%% @end
%%%-------------------------------------------------------------------

-module(simple_im_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).



%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    AppMode = env:get(app_mode),
    {ok, Pid} = simple_im_sup:start_link(AppMode),

    case AppMode of
        im ->
            ok = im:start();
        session_server ->
            ok = session_server:start();
        _ ->
            ok
    end,
    {ok, Pid}.

%%--------------------------------------------------------------------
stop(_State) ->
    case env:get(app_mode) of
        session_server ->
            ok = session_server:stop();
        _ ->
            ok
    end.