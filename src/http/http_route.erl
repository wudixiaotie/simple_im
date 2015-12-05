%% ===================================================================
%% Author xiaotie
%% 2015-10-14
%% http server route
%% ===================================================================

-module(http_route).

-export([path/0]).



%% ===================================================================
%% API functions
%% ===================================================================

path() ->
    [{'_', [
        {"/contact/[...]", contact_handler, []},
        {"/group/[...]", group_handler, []},
        {"/health", health_handler, []},
        {"/offline/[...]", offline_handler, []},
        {"/server/[...]", server_handler, []},
        {"/user/[...]", user_handler, []},
        {"/m/[...]", m_handler, []},
        {"/gm/[...]", gm_handler, []},
        {"/file/audio/[...]", cowboy_static, {dir, env:get(audio_dir)}},
        {"/file/video/[...]", cowboy_static, {dir, env:get(video_dir)}},
        {"/file/image/[...]", cowboy_static, {dir, env:get(image_dir)}}
    ]}].