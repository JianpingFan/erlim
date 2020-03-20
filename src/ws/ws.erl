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


websocket_init(State) ->
  {[], State}.

websocket_handle({text, Info}, [#state{user_id = UserID}] = State) ->
  erlang:send(cache,{client_msg, UserID,Info}),
  {[], State};
websocket_handle(_Data, State) ->
  {[], State}.

websocket_info({reply, Msg}, State) ->
  {[{text, Msg}], State};

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

