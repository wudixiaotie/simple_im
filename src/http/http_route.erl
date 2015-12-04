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
        {"/file/[...]", file_handler, []},
        {"/user/[...]", user_handler, []}
    ]}].