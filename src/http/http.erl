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
-include("def_include.hrl").
%% API
-export([init/2]).
-compile(export_all).
ets_user_online()->
    ets:new(?ETS_USER_ONLINE,[set,public,named_table,{keypos,#user_online.token},{write_concurrency, true}, {read_concurrency, true}]),
ok.

init(Req0, Opts) ->
    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<$*>>, Req0),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>,Req2),
    Req =
    case cowboy_req:binding(action,Req3) of
        ?undefined->
            cowboy_req:reply(404, [], <<"Missing echo parameter.">>, Req3);
        ActionBin->
            ActionAtom = list_to_atom(binary_to_list(ActionBin)),
            handle_request:ActionAtom(Req3)
    end,
    {ok, Req, Opts}.

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain; charset=utf-8">>
    }, Echo, Req).


get_max_user_id()->
    AllData = cache:get_table_data(db:table_atom_name(?table_users)),
    log4erl:info("get_max_user_id AllData = ~w",[AllData]),
    lists:foldl(
        fun(#field_users{user_id = UserID},AccNum)->
            case UserID > AccNum of
                true->UserID;
                _->AccNum
            end
        end,
        0,
        AllData
    ).


generate_token_id()->
    util:random_int(1000000,9999999).

get_user_online(TokenID)->
    case ets:lookup(?ETS_USER_ONLINE,TokenID) of
        [UserOnline] when is_record(UserOnline,user_online)->UserOnline;
        _->false
    end.

get_user_id(TokenID) when is_list(TokenID)->
    get_user_id(list_to_integer(TokenID));
get_user_id(TokenID) when is_integer(TokenID)->
    case get_user_online(TokenID) of
        #user_online{user_id = UserID}->UserID;
        _->false
    end;
get_user_id(_Req)->false.


update_user_token(UserID,TokenID) when is_integer(UserID) andalso is_integer(TokenID)->
    case get_user_online(TokenID) of
        UserOnline when is_record(UserOnline,user_online)->
            ets:insert(?ETS_USER_ONLINE,UserOnline#user_online{token = TokenID});
        Err->
            log4erl:warn("get_user_online Err = ~w",[Err]),
            ets:insert(?ETS_USER_ONLINE,#user_online{token = TokenID,user_id = UserID})
    end;
update_user_token(UserID,TokenID) ->
    log4erl:error("update_user_token UserID = ~w,TokenID = ~w",[UserID,TokenID]),
    false.

