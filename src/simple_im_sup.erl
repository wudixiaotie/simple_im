-module(simple_im_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), #{id        => I,
                          start     => {I, start_link, []},
                          restart   => permanent,
                          type      => Type}).

-define(AGENT_CHILD(FUN), #{id      => agent,
                            start   => {agent, FUN, []},
                            restart => permanent,
                            type    => worker}).

-define(HTTP_CHILD(Name, Mod, Args, Type), #{id      => Name,
                                             start   => {Mod, start_link, Args},
                                             restart => permanent,
                                             type    => Type}).



%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, AppMode} = application:get_env(simple_im, app_mode),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [AppMode]).



%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([im]) ->
    {ok, { {one_for_one, 5, 10},
           [?CHILD(postgresql, worker),
            ?CHILD(redis, worker),
            ?CHILD(session, worker),
            ?CHILD(cf, worker),
            ?CHILD(client_sup, supervisor),
            ?CHILD(listener, worker),
            ?AGENT_CHILD(work_for_hunter)]} };
init([http]) ->
    {ok, { {one_for_one, 5, 10},
           [?HTTP_CHILD(postgresql, postgresql, [http], worker),
            ?HTTP_CHILD(redis, redis, [], worker),
            ?HTTP_CHILD(ranch, dependant, [ranch], supervisor),
            ?HTTP_CHILD(cowboy_app, dependant, [cowboy_app], supervisor),
            ?HTTP_CHILD(cowboy_http, dependant, [cowboy_http], worker),
            ?AGENT_CHILD(work_for_master)]} };
init([middleman]) ->
    {ok, { {one_for_one, 5, 10},
           [?CHILD(middleman_worker_sup, supervisor),
            ?CHILD(middleman_listener, worker)]} }.