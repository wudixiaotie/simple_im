%% ===================================================================
%% Author xiaotie
%% 2015-08-02
%% simple_one_for_one supervisor for client server
%% ===================================================================

-module(client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD, #{id         => client,
                 start      => {client, start_link, []},
                 restart    => temporary,
                 type       => worker}).



%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 0, 1}, [?CHILD] } }.