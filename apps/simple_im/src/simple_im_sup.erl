%%%-------------------------------------------------------------------
%% @doc simple_im top level supervisor.
%% @end
%%%-------------------------------------------------------------------

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



%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    AppMode = env:get(app_mode),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [AppMode]).



%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([im]) ->
    {ok, { {one_for_one, 5, 10},
           [?CHILD(log_server, worker),
            ?CHILD(postgresql, worker),
            ?CHILD(redis, worker),
            ?CHILD(session, worker),
            ?CHILD(client_sup, supervisor),
            ?CHILD(listener_sup, supervisor),
            ?AGENT_CHILD(work_for_hunter)]} };
init([http]) ->
    {ok, { {one_for_one, 5, 10},
           [?CHILD(log_server, worker),
            ?CHILD(postgresql, worker),
            ?CHILD(redis, worker),
            ?CHILD(http, worker),
            ?AGENT_CHILD(work_for_master)]} };
init([middleman]) ->
    {ok, { {one_for_one, 5, 10},
           [?CHILD(log_server, worker),
            ?CHILD(middleman_worker_sup, supervisor),
            ?CHILD(middleman_listener, worker)]} }.



%%====================================================================
%% Internal functions
%%====================================================================
