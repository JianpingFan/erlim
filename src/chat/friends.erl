%%%-------------------------------------------------------------------
%%% @author fanjianping
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 3月 2020 10:17
%%%-------------------------------------------------------------------
-module(friends).
-author("fanjianping").
-include("def_include.hrl").
%% API
-export([cs_frends_all_friends/2]).

cs_frends_all_friends(UserID,#cs_frends_all_friends{})->
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
    ?unicast(UserID,#sc_frends_all_friends{all_frends = AllFrends}),
    ok.
