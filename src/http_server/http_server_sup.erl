-module(http_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Mod, Args, Type), #{id      => Name,
                                        start   => {Mod, start_link, Args},
                                        restart => permanent,
                                        type    => Type}).



%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10},
           [?CHILD(postgresql, postgresql, [http], worker),
            ?CHILD(redis, redis, [], worker),
            ?CHILD(ranch, dependant, [ranch], supervisor),
            ?CHILD(cowboy_app, dependant, [cowboy_app], supervisor),
            ?CHILD(cowboy_http, dependant, [cowboy_http], worker)]} }.