%%%-------------------------------------------------------------------
%%% @author 89710
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 2月 2020 20:00
%%%-------------------------------------------------------------------
-module(ws).
-author("89710").
-include("def_include.hrl").

%% API
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-export([update_user_ws_pid/2,get_user_ws_pid/1]).
-record(state,{
  token = ?undefined,
  user_id = 0
}).


init(Req, Opts) ->
  Token = cowboy_req:binding(token,Req),
  case http:get_user_id(binary_to_integer(Token)) of
    UserID when is_integer(UserID) andalso UserID =/= 0->
      update_user_ws_pid(Token,self()),
      log4erl:info("websocket init success"),
      {cowboy_websocket, Req, [#state{token = binary_to_integer(Token),user_id = UserID}|Opts]};
    _->
      log4erl:info("websocket init fail Token = ~w",[Token]),
      {ok, Req, Opts}
  end.

%% 获取协议指定的模块名 如 cs_friend   return friend
get_module_atom_name(Message)->
  ModelAtom = element(1, Message),
  ModelString = atom_to_list(ModelAtom),
  ModelNameString = lists:sublist(ModelString,4,length(ModelString)),
  log4erl:info("get_modle_atom_name ModelNameString = ~s",[ModelNameString]),
  list_to_atom(ModelNameString).

handle_message(UserID,Message)->
  Module = get_module_atom_name(Message),
  [_|List] = tuple_to_list(Message),
  lists:foreach(
    fun(Tup)->
      Fun = element(1, Tup),
      log4erl:info("handle_message Module = ~w",[Module]),
      log4erl:info("handle_message Fun = ~w",[Fun]),
      log4erl:info("handle_message Tup = ~w",[Tup]),
      Module:Fun(UserID,Tup)
    end,
    List
  ),
  ok.

websocket_init(State) ->
  {[], State}.
websocket_handle({binary, UpBin}, [#state{user_id = UserID}] = State) ->
  log4erl:info("websocket recive UpBin = ~w",[UpBin]),
  case im_pb:decode_msg(UpBin,up) of
    UpTup when is_record(UpTup,up)->
      log4erl:info("websocket recive Up = ~w",[UpTup]),
      [_|UpList] = tuple_to_list(UpTup),
      [handle_message(UserID,Message)||Message<-UpList];
    _->
      log4erl:error("decode_msg bin error UserID = ~w,UpBin = ~w",[UserID,UpBin])
  end,
  {[], State};
websocket_handle(Data, State) ->
  log4erl:info("websocket recive Data = ~w",[Data]),
  {[], State}.

websocket_info({reply, Msg}, State) when is_binary(Msg)->
  {[{text, Msg}], State};
websocket_info({reply, Msg}, State) when is_tuple(Msg)->
  log4erl:info("reply Msg = ~w",[Msg]),
  {[{text, im_pb:encode_msg(Msg)}], State};

websocket_info({timeout, _Ref, _Msg}, State) ->
  {[], State};
websocket_info(_Info, State) ->
  {[], State}.





update_user_ws_pid(TokenID, WsPID) when is_integer(TokenID) andalso is_pid(WsPID) ->
  case http:get_user_online(TokenID) of
    UserOnline when is_record(UserOnline, user_online) ->
      ets:insert(?ETS_USER_ONLINE, UserOnline#user_online{ws_pid = WsPID});
    Err ->
      log4erl:error("get_user_online Err = ~w", [Err]),
      false
  end;
update_user_ws_pid(UserID, WsPID) ->
  log4erl:error("update_user_token UserID = ~w,WsPID = ~w", [UserID, WsPID]),
  false.




get_user_ws_pid(TokenID) when is_list(TokenID)->
  get_user_ws_pid(list_to_integer(TokenID));
get_user_ws_pid(TokenID) when is_integer(TokenID)->
  case http:get_user_online(TokenID) of
    #user_online{ws_pid = WsPid}->WsPid;
    _->false
  end;
get_user_ws_pid(_Req)->false.

