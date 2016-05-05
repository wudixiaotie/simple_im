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
        {ok, UserId, DeviceName} ->
            {ok, PostVals, _} = cowboy_req:body_qs(Req),
            case lists:keyfind(<<"name">>, 1, PostVals) of
                {<<"name">>, GroupName} ->
                    case get_members(PostVals) of
                        {ok, Members} ->
                            {ok, GroupId, Key} = groups:create(GroupName, UserId, Members),

                            Attrs = [{<<"t">>, <<"create_group">>},
                                     {<<"g_id">>, GroupId},
                                     {<<"g_key">>, Key},
                                     {<<"from">>, UserId},
                                     {<<"d">>, DeviceName}],
                            {ok, N} = handler_helper:complete_group_notification(Attrs),
                            {ok, NBin} = toml:term_2_binary(N),
                            ok = agent:notify(NBin),
                            {ok, TomlBin} = handler_helper:success();
                        {error, Reason} ->
                            {ok, TomlBin} = handler_helper:error(3, Reason)
                    end;
                _ ->
                    {ok, TomlBin} = handler_helper:error(3, <<"name Required">>)
            end
    end,
    handler_helper:return(200, TomlBin, Req);
handle_request([GroupIdBin], <<"DELETE">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, UserId, DeviceName} ->
            GroupId = erlang:binary_to_integer(GroupIdBin),
            ok = groups:delete(GroupId, UserId),
            Attrs = [{<<"t">>, <<"delete_group">>},
                     {<<"g_id">>, GroupId},
                     {<<"from">>, UserId},
                     {<<"d">>, DeviceName}],
            {ok, N} = handler_helper:complete_group_notification(Attrs),
            {ok, NBin} = toml:term_2_binary(N),
            ok = agent:notify(NBin),
            {ok, TomlBin} = handler_helper:success()
    end,
    handler_helper:return(200, TomlBin, Req);
handle_request([GroupIdBin, <<"member">>], <<"POST">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, UserId, DeviceName} ->
            {ok, PostVals, _} = cowboy_req:body_qs(Req),
            case utility:check_parameters([<<"g_key">>], PostVals) of
                {ok, [Key]} ->
                    GroupId = erlang:binary_to_integer(GroupIdBin),
                    case group_members:create_by_key(GroupId, Key, UserId) of
                        ok ->
                            Attrs = [{<<"t">>, <<"create_group_member">>},
                                     {<<"g_id">>, GroupId},
                                     {<<"member_id">>, UserId},
                                     {<<"from">>, UserId},
                                     {<<"d">>, DeviceName}],
                            {ok, N} = handler_helper:complete_group_notification(Attrs),
                            {ok, NBin} = toml:term_2_binary(N),
                            ok = agent:notify(NBin),
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
    handler_helper:return(200, TomlBin, Req);
handle_request([GroupIdBin, <<"member">>, MemberIdBin], <<"POST">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, UserId, DeviceName} ->
            GroupId = erlang:binary_to_integer(GroupIdBin),
            MemberId = erlang:binary_to_integer(MemberIdBin),
            case group_members:create_by_creator(GroupId, UserId, MemberId) of
                ok ->
                    Attrs = [{<<"t">>, <<"create_group_member">>},
                             {<<"g_id">>, GroupId},
                             {<<"member_id">>, MemberId},
                             {<<"from">>, UserId},
                             {<<"d">>, DeviceName}],
                    {ok, N} = handler_helper:complete_group_notification(Attrs),
                    {ok, NBin} = toml:term_2_binary(N),
                    ok = agent:notify(NBin),
                    {ok, TomlBin} = handler_helper:success();
                {error, group_not_exist} ->
                    {ok, TomlBin} = handler_helper:error(3, <<"Group Not Exist!">>);
                {error, unauthorized} ->
                    {ok, TomlBin} = handler_helper:error(3, <<"Unauthorized">>);
                _ ->
                    {ok, TomlBin} = handler_helper:error(3, <<"Unknow Error">>)
            end
    end,
    handler_helper:return(200, TomlBin, Req);
handle_request([GroupIdBin, <<"member">>], <<"DELETE">>, Req) ->
    case handler_helper:verify_token(Req) of
        {error, TomlBin} ->
            ok;
        {ok, UserId, DeviceName} ->
            GroupId = erlang:binary_to_integer(GroupIdBin),
            ok = group_members:delete(GroupId, UserId),
            Attrs = [{<<"t">>, <<"delete_group_member">>},
                     {<<"g_id">>, GroupId},
                     {<<"member_id">>, UserId},
                     {<<"from">>, UserId},
                     {<<"d">>, DeviceName}],
            {ok, N} = handler_helper:complete_group_notification(Attrs),
            {ok, NBin} = toml:term_2_binary(N),
            ok = agent:notify(NBin),
            {ok, TomlBin} = handler_helper:success()
    end,
    handler_helper:return(200, TomlBin, Req);
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