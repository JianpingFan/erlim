%%%-------------------------------------------------------------------
%%% @author 89710
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 2月 2020 20:00
%%%-------------------------------------------------------------------
-module(erlim_ws_handler).
-author("89710").
-include("def_include.hrl").

%% API
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-record(state,{
  token = ?undefined,
  user_id = 0
}).

init(Req, Opts) ->
  Token = cowboy_req:binding(token,Req),
  case http:get_user_id(binary_to_integer(Token)) of
    UserID when is_integer(UserID) andalso UserID =/= 0->
      log4erl:info("websocket init success"),
      {cowboy_websocket, Req, [#state{token = binary_to_integer(Token),user_id = UserID}|Opts]};
    _->
      log4erl:info("websocket init fail Token = ~w",[Token]),
      {ok, Req, Opts}
  end.


websocket_init(State) ->
  {[], State}.

websocket_handle({text, <<"user_create1">>}, State) ->
  erlang:send(login,{user_create,<<"fjp1">>}),
  {[], State};
websocket_handle({text, <<"user_create2">>}, State) ->
  erlang:send(login,{user_create,<<"fjp2">>}),
  {[], State};
websocket_handle({text, <<"user_login1">>}, State) ->
  erlang:send(login,{user_login,self(),"fjp1"}),
  {[], State};
websocket_handle({text, <<"user_login2">>}, State) ->
  erlang:send(login,{user_login,self(),"fjp2"}),
  {[], State};
websocket_handle({text, <<"8 ",Msg/binary>>}, State) ->
  erlang:send(chat_single,{text,self(),8,Msg}),
  {[], State};
websocket_handle({text, <<"7 ",Msg/binary>>}, State) ->
  erlang:send(chat_single,{text,self(),7,Msg}),
  {[], State};
%%websocket_handle({text, Msg}, State) ->
%%  {[{text, << "That's what she said! ", Msg/binary >>}], State};
websocket_handle(_Data, State) ->
  {[], State}.

websocket_info({login_return, UserID}, State)->
  log4erl:info("login_return UserID = ~w ",[UserID]),
  erlang:send(chat_single,{save_ws_pid,UserID,self()}),
  {[{text,integer_to_list(UserID)}], State};
websocket_info({reply, Msg}, State) ->
  {[{text, Msg}], State};

websocket_info({timeout, _Ref, Msg}, State) ->
%%  erlang:start_timer(1000, self(), <<"How' you doin'?">>),
  {[], State};
websocket_info(_Info, State) ->
  {[], State}.
