%%%-------------------------------------------------------------------
%%% @author fanjianping
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 3月 2020 16:23
%%%-------------------------------------------------------------------
-module(chat).
-author("fanjianping").
-include("def_include.hrl").
-define(msg_id_index,msg_id_index).
%% API
%%-export([cs_chat_single/2]).
%%
%%cs_chat_single(UserID,#cs_chat_single{receiver_id = ReceiverID,msg = Msg})->
%%    case catch check_chat_single(UserID,ReceiverID,Msg) of
%%        {error,Err}->
%%            ?unicast(UserID,#sc_chat_single{result = Err});
%%        _->
%%            ?unicast(ReceiverID,#sc_chat_msg_notify{sender_id = UserID,group_id = 0,msg_id = 0,time = util:now_mili(),msg = Msg}),
%%            ?unicast(UserID,#sc_chat_single{result = 1,msg_id = 0})
%%    end,
%%    ok.
%%
%%check_chat_single(_UserID,ReceiverID,#p_msg{msg_type = MsgType,text_msg = _TextMsg,pic = _Pic}) when ReceiverID =/= 0->
%%    case lists:member(MsgType,?MSG_ALL_TYPE) of
%%        false-> throw({error,2});
%%        _->ok
%%    end;
%%check_chat_single(_UserID,ReceiverID,_)->
%%    log4erl:error("check_chat_single ReceiverID = ~w",[ReceiverID]),
%%    throw({error,1}).
