%% ===================================================================
%% Author xiaotie
%% 2015-9-24
%% server handler
%% ===================================================================

-module (server_handler).

-export([init/2, handle_request/3]).

-include("user.hrl").



%% ===================================================================
%% API functions
%% ===================================================================

init(Req, Opts) ->
    handler_helper:init(?MODULE, Req, Opts).



%% ===================================================================
%% Request handler
%% ===================================================================

handle_request([<<"login">>], <<"POST">>, Req) ->
    {ok, PostVals, _} = cowboy_req:body_qs(Req),
    Toml = case utility:check_parameters([<<"phone">>, <<"password">>], PostVals) of
        {ok, [Phone, Password]} ->
            case users:verify(Phone, Password) of
                {ok, true, UserId} ->
                    {ok, Token} = utility:guid(),
                    [IP, Port] = get_node(),
                    {ok, TokenKey} = redis:key({token, Token}),
                    {ok, <<"OK">>} = redis:q([<<"HMSET">>, TokenKey,
                                              <<"ip">>, IP, <<"port">>, Port,
                                              <<"user_id">>, UserId]),
                    {ok, <<"1">>} = redis:q([<<"EXPIRE">>, TokenKey, 100]),
                    {<<"response">>, [{<<"status">>, 0},
                                      {<<"server">>, IP},
                                      {<<"port">>, Port},
                                      {<<"user">>, [{<<"token">>, Token},
                                                    {<<"id">>, UserId}]}]};
                {ok, false} ->
                    {<<"response">>, [{<<"status">>, 1},
                                      {<<"reason">>, <<"Mismatch">>}]};
                {error, _Reason} ->
                    {<<"response">>, [{<<"status">>, 2},
                                      {<<"reason">>, <<"Unkown Error">>}]}
            end;
        {error, Reason} ->
            {<<"response">>, [{<<"status">>, 3},
                              {<<"reason">>, Reason}]}
    end,
    {ok, TomlBin} = toml:term_2_binary(Toml),
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request([<<"reconnect">>], <<"POST">>, Req) ->
    [{<<"token">>, Token}] = cowboy_req:parse_cookies(Req),
    {ok, [{<<"id">>, UserIdBin}], _} = cowboy_req:body_qs(Req),
    {ok, TokenKey} = redis:key({token, Token}),
    Result = redis:q([<<"HMGET">>, TokenKey, <<"ip">>, <<"port">>, <<"user_id">>]),
    Toml = case Result of
        {ok, [undefined, undefined, undefined]} ->
            {<<"response">>, [{<<"status">>, 1}]};
        {ok, [IP, Port, UserIdBin]} ->
            {<<"response">>, [{<<"status">>, 0},
                              {<<"server">>, IP},
                              {<<"port">>, Port}]};
        _ ->
            {<<"response">>, [{<<"status">>, 2}]}
    end,
    {ok, TomlBin} = toml:term_2_binary(Toml),
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request([<<"failed">>], <<"POST">>, Req) ->
    PostVals = cowboy_req:parse_cookies(Req),
    {<<"token">>, Token} = lists:keyfind(<<"token">>, 1, PostVals),
    {ok, TokenKey} = redis:key({token, Token}),
    Result = redis:q([<<"HMGET">>, TokenKey, <<"ip">>, <<"port">>]),
    Toml = case Result of
        {ok, [undefined, undefined]} ->
            {<<"response">>, [{<<"status">>, 1}]};
        {ok, [IP, Port]} ->
            case gen_tcp:connect(erlang:binary_to_list(IP),
                                 erlang:binary_to_integer(Port), []) of
                {ok, Socket} ->
                    gen_tcp:close(Socket),
                    {<<"response">>, [{<<"status">>, 3},
                                      {<<"r">>, <<"IM is online">>}]};
                {error, _Reason} ->
                    {ok, IMListKey} = redis:key(im_list),
                    redis:q([<<"HDEL">>, IMListKey, utility:ip_port(IP, Port)]),
                    [NewIP, NewPort] = get_node(),
                    {ok, TokenKey} = redis:key({token, Token}),
                    redis:q([<<"HMSET">>, TokenKey,
                             <<"ip">>, NewIP, <<"port">>, NewPort]),
                    {<<"response">>, [{<<"status">>, 0},
                                      {<<"server">>, NewIP},
                                      {<<"port">>, NewPort}]};
                _ ->
                    {<<"response">>, [{<<"status">>, 2}]}
            end;
        _ ->
            {<<"response">>, [{<<"status">>, 2}]}
    end,
    {ok, TomlBin} = toml:term_2_binary(Toml),
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request(_, _, Req) ->
    handler_helper:return404(Req).



%% ===================================================================
%% Internal functions
%% ===================================================================


get_node() ->
    {ok, IMList} = redis:q([<<"HKEYS">>, <<"im_list">>]),
    {ok, Index} = utility:random_number(erlang:length(IMList)),
    IM = lists:nth(Index, IMList),
    IPPort = erlang:binary_to_list(IM),
    re:split(IPPort, ":").