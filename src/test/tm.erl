%% ===================================================================
%% Author xiaotie
%% 2015-08-02
%% test mnesia
%% ===================================================================

-module (tm).

-behaviour (gen_server).

% APIs
-export([start_link/0, find/1, find1/1, ct/0, ct1/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (test_mnesia, {userid, pid}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
find(UserId) ->
    mnesia:dirty_read(test_mnesia, UserId).
find1(UserId) ->
    mnesia:dirty_index_read(test_mnesia, UserId, #test_mnesia.pid).
ct() ->
    tc:ct(tm, find, [<<"user_1@android">>], 100000).
ct1() ->
    tc:ct(tm, find1, [<<"user_1@android">>], 100000).
    
    
%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    {atomic,ok} = mnesia:create_table(test_mnesia, [{ram_copies, [node()]},
                                                    {attributes, record_info(fields, test_mnesia)}]),
    {atomic,ok} = mnesia:add_table_index(test_mnesia, pid),
    loop(10000000),
    {ok, []}.
handle_call(_Request, _From, State) ->
    {reply, nomatch, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVer, State, _Extra) -> {ok, State}.


%% ===================================================================
%% Internal functions
%% ===================================================================
loop(N) when N > 0 ->
    UserId = <<"user_", (integer_to_binary(N))/binary, "@android">>,
    mnesia:dirty_write(#test_mnesia{userid = UserId, pid = UserId}),
    loop(N - 1);
loop(0) ->
    ok.