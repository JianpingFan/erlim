%%%-------------------------------------------------------------------
%%% @author 89710
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 2月 2020 19:17
%%%-------------------------------------------------------------------
-module(http).
-author("89710").
-define(SESSIONID,"sessionid").
-include("def_include.hrl").
%% API
-export([init/2,ets_user_online/0,get_user_id/1]).

ets_user_online()->
    ets:new(?ETS_USER_ONLINE,[set,public,named_table,{keypos,#user_online.token},{write_concurrency, true}, {read_concurrency, true}]),
ok.


init(Req0, Opts) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req0),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>,Req2),
    Method = cowboy_req:method(Req3),
%%    HasBody = cowboy_req:has_body(Req3),
    case cowboy_req:binding(action,Req3) of
        ?undefined->maybe_echo(Method, ?undefined, Req3);
        ActionBin->
            maybe_echo(Method, binary_to_list(ActionBin), Req3)
    end,
    Action = cowboy_req:binding(action,Req3),
    Req = maybe_echo(Method, Action, Req3),
    {ok, Req, Opts}.


maybe_echo(<<"POST">>, undefined, Req) ->
    echo(unicode:characters_to_binary("1"), Req);
maybe_echo(Method, undefined, Req) when Method == <<"GET">> orelse Method == <<"POST">> ->
    echo(unicode:characters_to_binary("1"), Req);
maybe_echo(<<"POST">>, "login", Req0) ->
    {ok, PostVals, Req} = cowboy_req:read_urlencoded_body(Req0),
    Mobile = proplists:get_value(<<"mobile">>, PostVals),
    Passwd = proplists:get_value(<<"passwd">>, PostVals),
    Sql = io_lib:format("select * from users where mobile = '~s'", [Mobile]),
    case gen_server:call(db, {select, list_to_binary(Sql)}) of
        [[IDBin, MobileBin, PwdBin, _NickNameBin, _CreateTimeBin]] ->
            case binary_to_integer(MobileBin) == binary_to_integer(Mobile) andalso binary_to_list(PwdBin) == binary_to_list(Passwd) of
                true ->
                    log4erl:info("login success"),
                    TokenID = generate_token_id(),
                    UserID = binary_to_integer(IDBin),
                    log4erl:info("UserID = ~w",[UserID]),
                    log4erl:info("token = ~w ",[TokenID]),
                    update_user_token(UserID,TokenID),
                    hook_login_success(UserID),
                    ReturnList = [{"res","0"},{"user_id",integer_to_list(UserID)},{"token",integer_to_list(TokenID)}],
                    log4erl:info("ReturnList = ~w",[ReturnList]),
                    json_return(ReturnList,Req);
                _ ->
                    echo(unicode:characters_to_binary("login fail"), Req)
            end;
        _ ->
            echo(unicode:characters_to_binary("login fail"), Req)
    end;
%%maybe_echo(<<"POST">>, false, Req) ->
%%    cowboy_req:reply(400, [], <<"Missing body.">>, Req);
maybe_echo(ReqWay, HasBody, Req) ->
    log4erl:info("ReqWay = ~w",[ReqWay]),
    log4erl:info("HasBody = ~w",[HasBody]),
    log4erl:info("Req = ~w",[Req]),
    %% Method not allowed.
    cowboy_req:reply(405, Req).

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain; charset=utf-8">>
    }, Echo, Req).



generate_token_id()->
    util:random_int(1000000,9999999).

get_user_id(TokenID) when is_list(TokenID)->
    get_user_id(list_to_integer(TokenID));
get_user_id(TokenID) when is_integer(TokenID)->
    case ets:lookup(?ETS_USER_ONLINE,TokenID) of
        [#user_online{user_id = UserID}]->UserID;
        _->false
    end;
get_user_id(_Req)->false.

update_user_token(UserID,TokenID) when is_integer(UserID) andalso is_integer(TokenID)->
    ets:insert(?ETS_USER_ONLINE,#user_online{user_id = UserID,token = TokenID}).

json_return(List,Req) when is_list(List)->
    Json = jsone:encode([{list_to_binary(Key),list_to_binary(Value)}||{Key,Value}<-List]),
    echo(Json, Req),
    ok;
json_return(List,Req)->
    log4erl:error("json_return List = ~w",[List]),
    echo(undefined, Req).

hook_login_success(UserID)->

    ok.
