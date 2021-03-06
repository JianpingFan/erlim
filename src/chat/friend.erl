%%%-------------------------------------------------------------------
%%% @author fanjianping
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 3月 2020 10:17
%%%-------------------------------------------------------------------
-module(friend).
-author("fanjianping").
-include("def_include.hrl").
-compile(export_all).
%% API
%%-export([cs_frends_all_friends/2]).
%%
handle_info({cs_fetch_all_friends,UserID})->
    cs_fetch_all_friends(UserID);
handle_info({login,MobileBin,PasswdBin})->
    handle_request:do_login(MobileBin,PasswdBin).

cs_fetch_all_friends(UserID)->
    UserList = cache:get_table_data(?table_users),
    AllFrends =
        lists:foldl(
            fun(#field_users{user_id = UID,nickname = NickName},Acc)->
                case UID of
                    UserID->Acc;
                    _->
                        [#p_frend{user_id = UID,nickname = NickName}|Acc]
                end
            end,
            [],
            UserList
        ),
    ?unicast(UserID,#down{friend_fun = #sc_friend{fetch_all_friends = #sc_fetch_all_friends{frends = AllFrends}}}),
    ok.
cs_fetch_all_friends(UserID,#cs_fetch_all_friends{})->
    erlang:send(cache,{?route,?MODULE,{cs_fetch_all_friends,UserID}}),
    ok.
