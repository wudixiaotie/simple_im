%% ===================================================================
%% Author xiaotie
%% 2015-10-24
%% group handler
%% ===================================================================

-module(group_handler).

-export([init/2, handle_request/3]).



%% ===================================================================
%% API functions
%% ===================================================================

init(Req, Opts) ->
    handler_helper:init(?MODULE, Req, Opts).



%% ===================================================================
%% Request handler
%% ===================================================================

handle_request([], <<"POST">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, UserId} ->
            {ok, PostVals, _} = cowboy_req:body_qs(Req),
            case lists:keyfind(<<"name">>, 1, PostVals) of
                {<<"name">>, GroupName} ->
                    case get_members(PostVals) of
                        {ok, Members} ->
                            {ok, GroupId, Key} = groups:create(GroupName, UserId, Members),

                            N = {<<"n">>, [{<<"t">>, <<"create_group">>},
                                           {<<"g_id">>, GroupId},
                                           {<<"g_key">>, Key},
                                           {<<"ts">>, utility:timestamp()}]},
                            {ok, NBin} = toml:term_2_binary(N),
                            ok = agent:offer_a_reward(NBin),
                            {ok, TomlBin} = handler_helper:success();
                        {error, Reason} ->
                            {ok, TomlBin} = handler_helper:error(3, Reason)
                    end;
                _ ->
                    {ok, TomlBin} = handler_helper:error(3, <<"name Required">>)
            end
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request([GroupIdBin], <<"DELETE">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, UserId} ->
            GroupId = erlang:binary_to_integer(GroupIdBin),
            ok = groups:delete(GroupId, UserId),
            N = {<<"n">>, [{<<"t">>, <<"delete_group">>},
                           {<<"g_id">>, GroupId},
                           {<<"ts">>, utility:timestamp()}]},
            {ok, NBin} = toml:term_2_binary(N),
            ok = agent:offer_a_reward(NBin),
            {ok, TomlBin} = handler_helper:success()
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request([GroupIdBin, <<"member">>], <<"POST">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, UserId} ->
            {ok, PostVals, _} = cowboy_req:body_qs(Req),
            case utility:check_parameters([<<"g_key">>], PostVals) of
                {ok, [Key]} ->
                    GroupId = erlang:binary_to_integer(GroupIdBin),
                    case group_members:create_by_key(GroupId, Key, UserId) of
                        ok ->
                            N = {<<"n">>, [{<<"t">>, <<"create_group_member">>},
                                           {<<"g_id">>, GroupId},
                                           {<<"gm_id">>, UserId},
                                           {<<"ts">>, utility:timestamp()}]},
                            {ok, NBin} = toml:term_2_binary(N),
                            ok = agent:offer_a_reward(NBin),
                            {ok, TomlBin} = handler_helper:success();
                        {error, group_not_exist} ->
                            {ok, TomlBin} = handler_helper:error(3, <<"Group Not Exist!">>);
                        {error, unauthorized} ->
                            {ok, TomlBin} = handler_helper:error(3, <<"Unauthorized">>);
                        _ ->
                            {ok, TomlBin} = handler_helper:error(3, <<"Unknow Error">>)
                    end;
                {error, Reason} ->
                    {ok, TomlBin} = handler_helper:error(3, Reason)
            end
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request([GroupIdBin, <<"member">>, MemberIdBin], <<"POST">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, UserId} ->
            GroupId = erlang:binary_to_integer(GroupIdBin),
            MemberId = erlang:binary_to_integer(MemberIdBin),
            case group_members:create_by_creator(GroupId, UserId, MemberId) of
                ok ->
                    N = {<<"n">>, [{<<"t">>, <<"create_group_member">>},
                                   {<<"g_id">>, GroupId},
                                   {<<"gm_id">>, MemberId},
                                   {<<"ts">>, utility:timestamp()}]},
                    {ok, NBin} = toml:term_2_binary(N),
                    ok = agent:offer_a_reward(NBin),
                    {ok, TomlBin} = handler_helper:success();
                {error, group_not_exist} ->
                    {ok, TomlBin} = handler_helper:error(3, <<"Group Not Exist!">>);
                {error, unauthorized} ->
                    {ok, TomlBin} = handler_helper:error(3, <<"Unauthorized">>);
                _ ->
                    {ok, TomlBin} = handler_helper:error(3, <<"Unknow Error">>)
            end
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request([GroupIdBin, <<"member">>], <<"DELETE">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, UserId} ->
            GroupId = erlang:binary_to_integer(GroupIdBin),
            ok = group_members:delete(GroupId, UserId),
            N = {<<"n">>, [{<<"t">>, <<"delete_group_member">>},
                           {<<"g_id">>, GroupId},
                           {<<"gm_id">>, UserId},
                           {<<"ts">>, utility:timestamp()}]},
            {ok, NBin} = toml:term_2_binary(N),
            ok = agent:offer_a_reward(NBin),
            {ok, TomlBin} = handler_helper:success()
    end,
    cowboy_req:reply(200, [], TomlBin, Req);
handle_request(_, _, Req) ->
    handler_helper:return404(Req).



%% ===================================================================
%% Internal functions
%% ===================================================================

get_members(PostVals) ->
    get_members(PostVals, []).


get_members([{<<"members[]">>, Value}|T], Result) ->
    get_members(T, [Value|Result]);
get_members([_|T], Result) ->
    get_members(T, Result);
get_members([], []) ->
    {error, <<"members Required!">>};
get_members([], Result) ->
    {ok, Result}.