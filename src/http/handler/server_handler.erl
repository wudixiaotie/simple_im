%% ===================================================================
%% Author xiaotie
%% 2015-9-24
%% server handler
%% ===================================================================

-module(server_handler).

-export([init/2, handle_request/3]).



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
    case utility:check_parameters([<<"phone">>, <<"password">>], PostVals) of
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
                    Toml = {<<"response">>, [{<<"status">>, 0},
                                             {<<"server">>, IP},
                                             {<<"port">>, Port},
                                             {<<"user">>, [{<<"token">>, Token},
                                                           {<<"id">>, UserId}]}]},
                    {ok, TomlBin} = toml:term_2_binary(Toml);
                {ok, false} ->
                    {ok, TomlBin} = handler_helper:error(1, <<"Mismatch">>);
                {error, user_does_not_exist} ->
                    {ok, TomlBin} = handler_helper:error(2, <<"User doesn't exist!">>);
                {error, Reason} ->
                    log:e("[HTTP] Login Error: ~p~n", [Reason]),
                    {ok, TomlBin} = handler_helper:error(3, <<"Unkown Error">>)
            end;
        {error, Reason} ->
            log:e("[HTTP] Login Error: ~p~n", [Reason]),
                    {ok, TomlBin} = handler_helper:error(3, Reason)
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request([<<"reconnect">>], <<"POST">>, Req) ->
    [{<<"token">>, Token}] = cowboy_req:parse_cookies(Req),
    {ok, [{<<"id">>, UserIdBin}], _} = cowboy_req:body_qs(Req),
    {ok, TokenKey} = redis:key({token, Token}),
    Result = redis:q([<<"HMGET">>, TokenKey, <<"ip">>, <<"port">>, <<"user_id">>]),
    case Result of
        {ok, [undefined, undefined, undefined]} ->
             {ok, TomlBin} = handler_helper:error(1, <<"Token Error">>);
        {ok, [IP, Port, UserIdBin]} ->
            Toml = {<<"response">>, [{<<"status">>, 0},
                                     {<<"server">>, IP},
                                     {<<"port">>, Port}]},
            {ok, TomlBin} = toml:term_2_binary(Toml);
        _ ->
            {ok, TomlBin} = handler_helper:error(2, <<"Unkown Error">>)
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request([<<"failed">>], <<"POST">>, Req) ->
    PostVals = cowboy_req:parse_cookies(Req),
    {<<"token">>, Token} = lists:keyfind(<<"token">>, 1, PostVals),
    {ok, TokenKey} = redis:key({token, Token}),
    Result = redis:q([<<"HMGET">>, TokenKey, <<"ip">>, <<"port">>]),
    case Result of
        {ok, [undefined, undefined]} ->
            {ok, TomlBin} = handler_helper:error(1, <<"Token Error">>);
        {ok, [IP, Port]} ->
            case gen_tcp:connect(erlang:binary_to_list(IP),
                                 erlang:binary_to_integer(Port), []) of
                {ok, Socket} ->
                    ok = gen_tcp:close(Socket),
                    {ok, TomlBin} = handler_helper:error(1, <<"IM is online">>);
                {error, _Reason} ->
                    {ok, IMListKey} = redis:key(im_list),
                    redis:q([<<"HDEL">>, IMListKey, utility:ip_port(IP, Port)]),
                    [NewIP, NewPort] = get_node(),
                    {ok, TokenKey} = redis:key({token, Token}),
                    redis:q([<<"HMSET">>, TokenKey,
                             <<"ip">>, NewIP, <<"port">>, NewPort]),
                    Toml = {<<"response">>, [{<<"status">>, 0},
                                             {<<"server">>, NewIP},
                                             {<<"port">>, NewPort}]},
                    {ok, TomlBin} = toml:term_2_binary(Toml);
                _ ->
                    {ok, TomlBin} = handler_helper:error(2, <<"Unkown Error">>)
            end;
        _ ->
            {ok, TomlBin} = handler_helper:error(2, <<"Unkown Error">>)
    end,
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