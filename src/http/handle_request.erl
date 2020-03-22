%%%-------------------------------------------------------------------
%%% @author 89710
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 3月 2020 20:55
%%%-------------------------------------------------------------------
-module(handle_request).
-author("89710").
-include("def_include.hrl").
-compile(export_all).
%% API
-export([]).

handle_call({register,MobileBin,PasswdBin})->
    handle_request:do_register(MobileBin,PasswdBin);
handle_call({login,MobileBin,PasswdBin})->
    handle_request:do_login(MobileBin,PasswdBin).

do_login(MobileBin,PasswdBin)->
    case cache:fetch_data_by_field(?table_users,#field_users.mobile,binary_to_integer(MobileBin)) of
        [#field_users{pwd = PwdString,user_id = UserID}]->
            case PasswdBin == list_to_binary(PwdString) of
                true->
                    {ok,UserID};
                _->
                    {error,2}
            end;
        _->
            {error,1} %% 手机号已存在
    end.

do_register(MobileBin,PasswdBin)->
    case cache:fetch_data_by_field(?table_users,#field_users.mobile,binary_to_integer(MobileBin)) of
        []->
            case http:get_max_user_id() of
                MaxUserID when is_integer(MaxUserID) andalso MaxUserID > 0 ->
                    FieldUsers = #field_users{user_id = MaxUserID + 1,nickname = "",mobile = binary_to_integer(MobileBin),pwd = binary_to_list(PasswdBin),create_time = util:now(),pri_key = [MaxUserID + 1]},
                    cache:write_data(?table_users,FieldUsers),
                    {ok,MaxUserID + 1};
                _->{error,-1}
            end;
        _->
            {error,1} %% 手机号已存在
    end.


%%%%    Method = cowboy_req:method(Req3),
login(Req0)->
    {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
    MobileBin = proplists:get_value(<<"mobile">>, PostVals),
    PasswdBin = proplists:get_value(<<"passwd">>, PostVals),
    case gen_server:call(cache, {?route,?MODULE,{login,MobileBin,PasswdBin}}) of
        {ok,UserID}->
            log4erl:info("login success"),
            TokenID = http:generate_token_id(),
            log4erl:info("UserID = ~w",[UserID]),
            log4erl:info("token = ~w ",[TokenID]),
            http:update_user_token(UserID,TokenID),
            hook_login_success(UserID),
            ReturnList = [{"res","0"},{"user_id",integer_to_list(UserID)},{"token",integer_to_list(TokenID)}],
            log4erl:info("ReturnList = ~w",[ReturnList]),
            json_return(ReturnList,Req),
            ok;
        {error,Err}->
            ReturnList = [{"res",integer_to_list(Err)}],
            log4erl:info("login ReturnList = ~w",[ReturnList]),
            json_return(ReturnList,Req)
    end.

register(Req0) ->
    {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
    MobileBin = proplists:get_value(<<"mobile">>, PostVals),
    PasswdBin = proplists:get_value(<<"passwd">>, PostVals),
    case gen_server:call(cache, {?route,?MODULE,{register,MobileBin,PasswdBin}}) of
        {ok,UserID}->
            log4erl:info("register success"),
            TokenID = http:generate_token_id(),
            log4erl:info("UserID = ~w",[UserID]),
            log4erl:info("token = ~w ",[TokenID]),
            http:update_user_token(UserID,TokenID),
            hook_login_success(UserID),
            ReturnList = [{"res","0"},{"user_id",integer_to_list(UserID)},{"token",integer_to_list(TokenID)}],
            log4erl:info("register ReturnList = ~w",[ReturnList]),
            json_return(ReturnList,Req),
            ok;
        {error,Err}->
            ReturnList = [{"res",integer_to_list(Err)}],
            log4erl:info("ReturnList = ~w",[ReturnList]),
            json_return(ReturnList,Req)
    end.



json_return(List,Req) when is_list(List)->
    Json = jsone:encode([{list_to_binary(Key),list_to_binary(Value)}||{Key,Value}<-List]),
    http:echo(Json, Req),
    ok;
json_return(List,Req)->
    log4erl:error("json_return List = ~w",[List]),
    http:echo(undefined, Req).

hook_login_success(UserID)->

    ok.
