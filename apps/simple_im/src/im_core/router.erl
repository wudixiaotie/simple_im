%% ===================================================================
%% Author xiaotie
%% 2015-11-24
%% roster for im message transmission
%% ===================================================================

-module(router).

% APIs
-export([route_to_single_user/2, route_to_multiple_user/2, 
         route_to_multiple_user/3]).



%% ===================================================================
%% API functions
%% ===================================================================

route_to_single_user(UserId, Message) ->
    case session:find(UserId) of
        offline ->
            ok = offline:store(UserId, [Message]),
            log:i("[IM] Client store offline msg: ~p~n", [Message]);
        {ok, ToPid} ->
            ToPid ! Message;
        Reason ->
            ok = offline:store(UserId, [Message]),
            log:i("[IM] Find session error:~p store offline msg: ~p~n", [Reason, Message])
    end,
    ok.


route_to_multiple_user([UserId|T], Message) ->
    ok = route_to_single_user(UserId, Message),
    route_to_multiple_user(T, Message);
route_to_multiple_user([], _) ->
    ok.


route_to_multiple_user([BeIgnoredUserId|T], BeIgnoredUserId, Message) ->
    route_to_multiple_user(T, BeIgnoredUserId, Message);
route_to_multiple_user([UserId|T], BeIgnoredUserId, Message) ->
    ok = route_to_single_user(UserId, Message),
    route_to_multiple_user(T, BeIgnoredUserId, Message);
route_to_multiple_user([], _, _) ->
    ok.