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
        {"/health", health_handler, []},
        {"/server/[...]", server_handler, []},
        {"/offline/[...]", offline_handler, []},
        {"/contact/[...]", contact_handler, []},
        {"/user/[...]", user_handler, []},
        {"/upload/[...]", upload_handler, []}
    ]}].