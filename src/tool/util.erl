%% @author caohongyang
%% @doc test edoc.
%% created 2013-2-18
-module(util).
-compile(export_all).
-export([
  pow/1,
  log/5,
  now/0,
  now_mili/0,
  now_day/0,
  md5/1,
  ceil/1,
  floor/1,
  sleep/1,
  sleep/2,
  get_list/2,
  implode/2,
  implode/3,
  explode/2,
  explode/3,
  for/3,
  for/4,
  string_to_term/1,
  bitstring_to_term/1,
  term_to_string/1,
  term_to_bitstring/1,
  to_string/1,
  null_proc/0,
  to_atom/1,
  toSqlDate/1,
  toUnixTime/1,
  datetime_to_seconds/1,
  seconds_to_datetime/1,
  foldl/3,
  nth_take/2,
  fun_take/2,
  fun_find/2,
  fun_replace/3,
  keymax/2,
  keymin/2,
  is_duplicate/1,
  copy_list/2,
  to_list/1,
  append_kvList/2,
  append_kvList/1,
  nth/2,
  element_pos/2,
  random_int/2,
  random_list/2,
  random_list2/2,
  random_list2/1,
  random_weigh_list/2,
  random_weigh_list2/2,
  tc/2,
  ip_to_str/1,
  words_filter/1,
  check_blankName/1,
  calc_name_length/1,
  gen_utf8_decode_list/2,
  splitwith/2
]).

-export([sqlDayToDate/1
  ,dateToSqlDay/1
  ,int_format_two/1
]).

-export([
  read_timer/1
]).

-export([
  ets_foreach_key/2
  ,ets_all_key/1
]).

-export([
  count_src_line_num/1,
  latin1/1,
  is_name_en/1
]).
%-record(constant, {data1 =calendar:datetime_to_gregorian_seconds(data_settint:get( time_zone))} ).%{1970,1,1}, {7,0,0}
%-define(GREGORIAN_INTERVIAL_TIME,  ((#constant{})#constant.data1)  ).

-define(GREGORIAN_INTERVIAL_TIME,  (calendar:datetime_to_gregorian_seconds(data_setting:get( time_zone)))).


unicast(UserID, DataRecord) when is_integer(UserID) andalso is_tuple(DataRecord) ->
  case UserID of
    0->log4erl:error("unicast role_id=0, Data=~w",[DataRecord]);
    _->ignore
  end,
  case ws:get_user_ws_pid(UserID) of
    PID when is_pid(PID) ->
      erlang:send(PID,{reply,DataRecord});
    _ ->
      log4erl:error("unicast pid not found role_id=~w, Data=~w",[UserID,DataRecord])
  end;
unicast(_UserID, _DataRecord)->
  ignore.


%% 2的N次方
pow(N) ->
  pow(N,1).
pow(0,R) ->
  R;
pow(N,R)when N > 0 ->
  pow(N-1,2*R).

%% 在List中的每两个元素之间插入一个分隔符
implode(_S, [])->
  [<<>>];
implode(S, L) when is_list(L) ->
  implode(S, L, []).
implode(_S, [H], NList) ->
  lists:reverse([thing_to_list(H) | NList]);
implode(S, [H | T], NList) ->
  L = [thing_to_list(H) | NList],
  implode(S, T, [S | L]).

%% 字符->列
explode(S, B)->
  re:split(B, S, [{return, list}]).
explode(S, B, int) ->
  [list_to_integer(Str) || Str <- explode(S, B), length(Str) > 0].

thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X)   -> mochinum:digits(X);
thing_to_list(X) when is_atom(X)    -> atom_to_list(X);
thing_to_list(X) when is_binary(X)  -> binary_to_list(X);
thing_to_list(X) when is_list(X)    -> X.

%% 日志记录函数
%% log(T, F, A, Mod, Line) ->
%%     {ok, Fl} = file:open("logs/error_log.txt", [write, append]),
%%     Format = list_to_binary("#" ++ T ++" ~s[~w:~w] " ++ F ++ "\r\n"),
%%     {{Y, M, D},{H, I, S}} = erlang:localtime(),
%%     Date = list_to_binary([integer_to_list(Y),"-", integer_to_list(M), "-", integer_to_list(D), " ", integer_to_list(H), ":", integer_to_list(I), ":", integer_to_list(S)]),
%%     io:format(Fl, unicode:characters_to_list(Format), [Date, Mod, Line] ++ A),
%%     file:close(Fl).

log(_,_,_,_,_) ->
  ok.

%% @doc 取得当前的unix时间戳,单位：秒
now() ->
  erlang:system_time(1).

%% @doc 当前时间戳，单位：毫秒
now_mili() ->
  erlang:system_time(1000).

%%当前是第几天，从1970年1月1日起
now_day()->
  calendar:date_to_gregorian_days(date())-calendar:date_to_gregorian_days({1970,1,1}).

%%讲日期转化为第几天，1970年1月1日是第0天
date_to_day({Y,M,D})->
  calendar:date_to_gregorian_days({Y,M,D})-calendar:date_to_gregorian_days({1970,1,1}).

%%将日期转化为星期几，星期一：1
date_to_week_day({Y,M,D})->
  calendar:day_of_the_week({Y,M,D}).

%%两个日期之间相差多少天
dif_day_date(T1,T2) when is_integer(T1) andalso is_integer(T2)->
  {{Y1, M1, D1}, _} = util:seconds_to_datetime(T1),
  {{Y2, M2, D2}, _} = util:seconds_to_datetime(T2),
  DifDay=calendar:date_to_gregorian_days({Y2,M2,D2})-calendar:date_to_gregorian_days({Y1,M1,D1}),
  erlang:abs(DifDay);
dif_day_date({Y1,M1,D1},{Y2,M2,D2})->
  DifDay=calendar:date_to_gregorian_days({Y2,M2,D2})-calendar:date_to_gregorian_days({Y1,M1,D1}),
  erlang:abs(DifDay).

%%两个秒数相差天数
dif_day_by_sec(NowSec, LastSec)->
  dif_day_by_sec(NowSec, LastSec, {0,0,0}).
dif_day_by_sec(Sec1, Sec2, CriticalTime)->
  {Date1, Time1} = util:seconds_to_datetime(Sec1),
  {Date2, Time2} = util:seconds_to_datetime(Sec2),
  case Time1>=CriticalTime of
    true->
      Day1=calendar:date_to_gregorian_days(Date1);
    false->
      Day1=calendar:date_to_gregorian_days(Date1)-1
  end,
  case Time2>=CriticalTime of
    true->
      Day2=calendar:date_to_gregorian_days(Date2);
    false->
      Day2=calendar:date_to_gregorian_days(Date2)-1
  end,
  erlang:abs(Day2-Day1).

%% 转换成HEX格式的md5
md5(S) ->
  lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

%% 将binary打包成SQL语句中的HEX格式
to_hex(Binary) ->
  << <<(hd(integer_to_list(E,16))):8>> || <<E:4>> <= Binary>>.


to_hex2(Binary) ->
  << <<(integer_to_binary(E,16))/binary>> || <<E:4>> <= Binary>>.

t(N) ->
  B = list_to_binary(lists:duplicate(1000,123)),
  tc:run(N, fun() -> to_hex(B) end).
t2(N) ->
  B = list_to_binary(lists:duplicate(1000,123)),
  tc:run(N, fun() -> to_hex2(B) end).

%%X符号
get_symbol(X)->
  if
    X>=0->
      1;
    true->
      -1
  end.

%%向上取整
ceil(N) ->
  T = trunc(N),
  case N == T of
    true  -> T;
    false ->
      case N>=0 of
        true->1 + T;
        false->T
      end
  end.

%%向下取整
floor(X) ->
  T = trunc(X),
  case (X < T) of
    true -> T - 1;
    _ -> T
  end.

sleep(T) ->
  receive
  after T -> ok
  end.

sleep(T, F) ->
  receive
  after T -> F()
  end.

get_list([], _) ->
  [];
get_list(X, F) ->
  F(X).

%% for循环
for(Max, Max, F) ->
  F(Max);
for(I, Max, F)  when I =< Max ->
  F(I),
  for(I+1, Max, F);
for(_I, _Max, _F) ->
  nil.

%% 带返回状态的for循环
%% @return {ok, State}
for(Max, Min, _F, State) when Min<Max -> {ok, State};
for(Max, Max, F, State) -> F(Max, State);
for(I, Max, F, State)   -> {ok, NewState} = F(I, State), for(I+1, Max, F, NewState).

%% term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
term_to_string(Term) ->
  binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).

%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
term_to_bitstring(Term) ->
  erlang:list_to_bitstring(io_lib:format("~p", [Term])).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
  case erl_scan:string(String++".") of
    {ok, Tokens, _} ->
      case erl_parse:parse_term(Tokens) of
        {ok, Term} -> Term;
        _Err -> undefined
      end;
    _Error ->
      undefined
  end.

%% term反序列化，bitstring转换为term，e.g., <<"[{a},1]">>  => [{a},1]
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(BitString) ->
  string_to_term(binary_to_list(BitString)).

to_string(BitString) when is_bitstring(BitString)->
  binary_to_list(BitString);
to_string(Term) when is_atom(Term) ->
  atom_to_list(Term);
to_string(Term) when is_integer(Term) ->
  integer_to_list(Term);
to_string(Term) when is_list(Term) ->
  Term.

%% @doc convert other type to integer
-spec to_integer(Msg :: any()) -> integer().
to_integer(Msg) when is_integer(Msg) ->
  Msg;
to_integer(Msg) when is_binary(Msg) ->
  Msg2 = binary_to_list(Msg),
  list_to_integer(Msg2);
to_integer(Msg) when is_list(Msg) ->
  list_to_integer(Msg);
to_integer(Msg) when is_float(Msg) ->
  round(Msg);
to_integer(_Msg) ->
  throw(other_value).

list_to_atom(List) when is_list(List) ->
  case catch list_to_existing_atom(List) of
    Atom when is_atom(Atom) -> Atom;
    _ -> erlang:list_to_atom(List)
  end.

null_loop() ->
  receive
    _ ->
      null_loop()
  end.

null_proc() ->
  spawn(fun null_loop/0).

to_atom(A) when is_list(A) ->
  util:list_to_atom(A);
to_atom(A) when is_integer(A) ->
  util:list_to_atom(integer_to_list(A));
to_atom(A) when is_atom(A) ->
  A;
to_atom(A) ->
  [List] = io_lib:format("~w", [A]),
  util:list_to_atom(List).

datetime_to_seconds({_Date,_Time}=Datetime)->
  calendar:datetime_to_gregorian_seconds(Datetime)
    - ?GREGORIAN_INTERVIAL_TIME.

seconds_to_datetime(MTime)->
  calendar:gregorian_seconds_to_datetime(
    ?GREGORIAN_INTERVIAL_TIME+ MTime).

toSqlDate({A,B,_C}) when A < 1900->
  UnixTime = A *1000000 +B,
  toSqlDate(UnixTime);
toSqlDate({{A,B,C},{D,E,F}}) ->
  A*10000000000 + B*100000000 + C*1000000 + D*10000 + E*100 + F;
toSqlDate(UnixTime) when is_integer(UnixTime)->
  {{A,B,C},{D,E,F}} = seconds_to_datetime(UnixTime),
  A*10000000000 + B*100000000 + C*1000000 + D*10000 + E*100 + F.

int_format_two(A) ->
  lists:flatten(io_lib:format("~2..0w",[A])).

dateToSqlDay({A,B,C}) ->
  integer_to_list(A)++"-" ++ int_format_two(B) ++ "-" ++ int_format_two(C).

sqlDayToDate(String) ->
  case string:tokens(String, "-") of
    [A,B,C] ->
      {list_to_integer(A), list_to_integer(B), list_to_integer(C)};
    _ ->
      {0,0,0}
  end.

toUnixTime({A,B,_C}) ->
  A*1000000 + B;
toUnixTime({datetime, Time}) ->
  datetime_to_seconds(Time);
toUnixTime(SqlDate) ->
  A = SqlDate div 10000000000,
  Rest = SqlDate rem 10000000000,
  B = Rest div 100000000,
  Rest2 = Rest rem 100000000,
  C = Rest2 div 1000000,
  Rest3 = Rest2 rem 1000000,
  D = Rest3 div 10000,
  Rest4 = Rest3 rem 10000,
  E = Rest4 div 100,
  F = Rest rem 100,
  datetime_to_seconds({{A,B,C},{D,E,F}}).


foldl(_F, {return,Acc}, _L) ->
  Acc;
foldl(F, Acc, [Tail|L]) ->
  foldl(F,F(Tail,Acc), L);
foldl(F, Acc, []) when is_function(F,2)->
  Acc.

-define(seg_size, 16).
-define(max_seg, 32).
-define(expand_load, 5).
-define(contract_load, 3).
-define(exp_size, (?seg_size * ?expand_load)).
-define(con_size, (?seg_size * ?contract_load)).
-record(dict,
{size=0		      :: non_neg_integer(),   	% Number of elements
  n=?seg_size	      :: non_neg_integer(),   	% Number of active slots
  maxn=?seg_size	      :: non_neg_integer(),	% Maximum slots
  bso=?seg_size div 2  :: non_neg_integer(),   	% Buddy slot offset
  exp_size=?exp_size   :: non_neg_integer(),   	% Size to expand at
  con_size=?con_size   :: non_neg_integer(),   	% Size to contract at
  empty		      :: tuple(),		% Empty segment
  segs		      :: tuple()	      	% Segments
}).
-define(kv(K,V), [K|V]).			% Key-Value pair format
fold_dict(F, Acc, D) ->
  Segs = D#dict.segs,
  fold_segs(F, Acc, Segs, tuple_size(Segs)).

fold_segs(_, {return, Acc}, _, _) -> Acc;
fold_segs(F, Acc, Segs, I) when I >= 1 ->
  Seg = element(I, Segs),
  fold_segs(F, fold_seg(F, Acc, Seg, tuple_size(Seg)), Segs, I-1);
fold_segs(F, Acc, _, 0) when is_function(F, 3) -> Acc.

fold_seg(_, {return, _}=Acc, _, _) -> Acc;
fold_seg(F, Acc, Seg, I) when I >= 1 ->
  fold_seg(F, fold_bucket(F, Acc, element(I, Seg)), Seg, I-1);
fold_seg(F, Acc, _, 0) when is_function(F, 3) -> Acc.

fold_bucket(_, {return, _}=Acc, _) -> Acc;
fold_bucket(F, Acc, [?kv(Key,Val)|Bkt]) ->
  fold_bucket(F, F(Key, Val, Acc), Bkt);
fold_bucket(F, Acc, []) when is_function(F, 3) -> Acc.

to_list(A) when is_binary(A) ->
  binary_to_list(A);
to_list(A) when is_integer(A) ->
  integer_to_list(A);
to_list(A) when is_atom(A) ->
  atom_to_list(A);
to_list(A) when is_list(A) ->
  A.

%%连接两个列表，类似于：List1++List2
%%@return: NewList
append_list([], List2)->
  List2;
append_list(List1, [])->
  List1;
append_list(List1, List2)->
  append_list1(lists:reverse(List1), List2).
append_list1(List1, [Term|List2])->
  append_list1([Term|List1], List2);
append_list1(List, [])->
  lists:reverse(List).

append_kvList(List) ->
  lists:foldl(fun({K,V}, Acc) ->
    case lists:keytake(K, 1, Acc) of
      false ->
        [{K,V}|Acc];
      {value, {K,V1}, Acc1} ->
        [{K,V+V1}|Acc1]
    end
              end, [], List).

append_kvList(List1,List2) when length(List1) >= length(List2) ->
  append_kvList2(List2, List1);
append_kvList(List1, List2) ->
  append_kvList2(List1, List2).

append_kvList2([], List) ->
  List;
append_kvList2([KV | Rest], List) ->
  append_kvList2(Rest, insert_kv(KV, List)).

insert_kv({Key, Value}, List) ->
  case lists:keytake(Key, 1, List) of
    {value, {Key, OldValue}, List2} ->
      [{Key, OldValue+Value} | List2];
    false ->
      [{Key, Value} | List]
  end.

%% 超过范围返回最后一个，非列表时直接返回
nth(_N, [H]) ->H;
nth(1, [H|_]) -> H;
nth(N, [_|T]) when N > 1 ->
  nth(N - 1, T);
nth(_N, H) ->
  H.

%% 从一个带全中的列表中随机选取SelectNum个元素，可以相同
random_weigh_list2(List, SelectNum) ->
  TotalWeigh = get_total_weigh(List,0),
  random_weigh_list2(List, SelectNum, TotalWeigh, []).


random_weigh_list2(_List, 0, _, Result) ->
  Result;
random_weigh_list2(List, SelectNum, TotalWeigh, Result) ->
  Random = random:uniform() * TotalWeigh,
  {Weigh, _} = Element =
    foldl(fun({UnitWeigh, _}=Ele, Acc) ->
      if UnitWeigh + Acc > Random ->
        {return, Ele};
        true ->
          Acc + UnitWeigh
      end
          end, 0, List),
  random_weigh_list2(List, SelectNum -1, TotalWeigh, [Element| Result]).


%% 从一个带权重的列表List中随机选取SelectNum个不同元素，结果列表顺序随机
%% weighed_list:  [{weigh1, value1}, {weigh2, value2}]
random_weigh_list(List, SelectNum) ->
  Len = length(List),
  TotalWeigh = get_total_weigh(List,0),
  if Len =< SelectNum ->
    List;
    true ->
      random_weigh_list(List, SelectNum, TotalWeigh, [])
  end.


random_weigh_list(_List, 0, _, Result) ->
  Result;
random_weigh_list(List, SelectNum, TotalWeigh, Result) ->
  Random = random:uniform() * TotalWeigh,
  {Weigh, _} = Element =
    foldl(fun({UnitWeigh, _}=Ele, Acc) ->
      if UnitWeigh + Acc > Random ->
        {return, Ele};
        true ->
          Acc + UnitWeigh
      end
          end, 0, List),
  NewList = lists:delete(Element, List),
  random_weigh_list(NewList, SelectNum -1, TotalWeigh-Weigh, [Element| Result]).

random_one_from_weigh_list(List) ->
  TotalWeigh = get_total_weigh(List,0),
  Random = random:uniform() * TotalWeigh,
  foldl(fun({UnitWeigh, E}, Acc) ->
    if UnitWeigh + Acc >= Random ->
      {return, E};
      true ->
        Acc + UnitWeigh
    end
        end, 0, List).

random_take_one_from_weigh_list(List) ->
  TotalWeigh = get_total_weigh(List,0),
  Random = random:uniform() * TotalWeigh,
  random_take_one_from_weigh_list(List, Random, 0, []).
random_take_one_from_weigh_list([{_UnitWeigh, E}], _Random, _AccW, CheckList)->
  {E, CheckList};
random_take_one_from_weigh_list([{UnitWeigh, E}|RemList], Random, AccW, CheckList)->
  case UnitWeigh + AccW >= Random of
    true->
      {E, lists:reverse(CheckList)++RemList};
    false->
      random_take_one_from_weigh_list(RemList, Random, AccW+UnitWeigh, CheckList)
  end;
random_take_one_from_weigh_list([], _Random, _AccW, _CheckList)->
  throw({error, "random_take_one_from_empty_list"}).


random_one_from_list([Element]) ->
  Element;
random_one_from_list(List) ->
  Value = random:uniform(length(List)),
  lists:nth(Value,List).

random_take_one_from_list([])->
  throw({error,"random_take_one_from_empty_list"});
random_take_one_from_list([Element]) ->
  {Element,[]};
random_take_one_from_list(List) ->
  L=length(List),
  Value = random:uniform(L),
  {lists:nth(Value,List),lists:sublist(List, Value-1) ++ lists:sublist(List, Value+1, L-Value)}.

get_total_weigh([], Weigh) ->
  Weigh;
get_total_weigh([{WeighUnit, _} | Rest],Weigh) ->
  get_total_weigh(Rest, Weigh + WeighUnit).


random_list_quick(List) ->
  [Elem||{_,Elem}<-lists:keysort(1, [{random:uniform(), Elem}||Elem<-List])].
%% 将List的中元素随机排列
random_list(List) ->
  random_list(List, erlang:length(List), []).

random_list([], 0, RandomList) ->
  RandomList;
random_list(List, Len, AccRandomList) ->
  Elem = lists:nth(random:uniform(Len), List),
  random_list(lists:delete(Elem, List), Len - 1, [Elem|AccRandomList]).

%% 从列表List中随机选取SelectNum个元素，组成新的列表，新列表的元素排列顺序与其在List中顺序相同
random_list(List, SelectNum) ->
  Len = length(List),
  if Len =< SelectNum ->
    List;
    true ->
      random_list(List, SelectNum, Len, [])
  end.


random_list(_, 0, _, Result) ->
  lists:reverse(Result);
random_list([Head| Rest], SelectNum, Len, Result) ->
  case random:uniform() =< SelectNum / Len of
    true ->
      random_list(Rest, SelectNum-1, Len-1, [Head|Result]);
    false ->
      random_list(Rest, SelectNum, Len-1, Result)
  end.

%% 将一个列表元素随机一遍
random_list2(List) ->
  Len = length(List),
  random_list2(List, Len, Len, []).
%% 从一个列表中随机抽取N个，顺序随机 ，N可以超过界限
random_list2(List, N) ->
  random_list2(List, N, length(List),[]).

random_list2(_List, 0, _Length, Result) ->
  Result;
random_list2(List, N, Length, Result) ->
  if Length =:= 1 ->
    Select = hd(List),
    Rest = [],
    random_list2(Rest, N-1, Length-1, [Select|Result]);
    Length =:= 0 ->
      Result;
    true ->

      Rand = random:uniform(Length),
      {value, Select, Rest} = nth_take(Rand, List),
      random_list2(Rest, N-1, Length-1, [Select|Result])
  end.





%%删除随机,返回列表
random_list_unique(_List, 0) -> [];
random_list_unique(List, N) ->
  Length=length(List),
  case Length=<N of
    true -> List;
    false-> random_list_unique1(List, N, Length, [])
  end.
random_list_unique1([], _N, _Length, Result) -> Result;
random_list_unique1(_List, _N, 0, Result) -> Result;
random_list_unique1(_, N, _Length, Result) when N=<0 -> Result;
random_list_unique1(List, N, Length, Result) ->
  Random=util:random_int(1, Length),
  NewList=lists:sublist(List, 1, max(Random-1, 0)) ++ lists:sublist(List, Random+1, Length-Random),
  random_list_unique1(NewList, N-1, Length-1, [lists:nth(Random, List)|Result]).

%%从[Start, End]区间内取Num个随机数（随机数不重复）
random_num_list_unique(Start, End, Num) when Start=<End andalso Num>0->
  case End-Start>Num of
    true->
      random_num_list_unique_1(Start, End, Num, []);
    false->
      lists:seq(Start, End)
  end.
random_num_list_unique_1(_Start, _End, 0, List)->
  lists:sort(List);
random_num_list_unique_1(Start, End, Num, List)->
  Random=random_int(Start, End),
  case lists:member(Random, List) of
    true->
      random_num_list_unique_1(Start, End, Num, List);
    false->
      random_num_list_unique_1(Start, End, Num-1, [Random|List])
  end.

%% 计算一个元素在一个列表或tuple中第一次出现的位置
%% 元素在集合中不存在时，返回0
element_pos(Element, List) when is_list(List) ->
  element_pos(Element, List, 1);
element_pos(Element, Tuple) when is_tuple(Tuple) ->
  element_pos(Element, tuple_to_list(Tuple), 1);
element_pos(_, _) ->
  0.

element_pos(Element, [Element|_Rest], Index) ->
  Index;
element_pos(Element, [_|Rest], Index) ->
  element_pos(Element, Rest, Index+1);
element_pos(_, [], _) ->
  0.

%% 从[Lower...Higher]包括边界的整数区间中随机一个数
random_int(Int, Int)->
  Int;
random_int(Lower, Higher) when Lower =< Higher ->
  random:uniform(Higher -Lower+1) +Lower-1;
random_int(Higher, Lower) ->
  random_int(Lower, Higher).

fun_take(F,L) ->
  fun_take(F, L, []).

fun_take(F, [H|T], L)  ->
  case F(H) of
    true ->
      {value, H, lists:reverse(L, T)};
    false ->
      fun_take(F, T, [H|L])
  end;
fun_take(_F, [], _L) -> false.

fun_find(_F,[]) ->
  false;
fun_find(F,[E|Rest]) ->
  case F(E) of
    true ->
      E;
    _ ->
      fun_find(F,Rest)
  end.


fun_replace(F, [Tup|Tail], New) ->
  case F(Tup) of
    true ->
      [New|Tail];
    _ ->
      [Tup|fun_replace(F, Tail, New)]
  end;
fun_replace(_F, [], _) -> [].

%% 读timer
read_timer(Timer) when is_reference(Timer)->
  case erlang:read_timer(Timer) of
    false ->
      0;
    A ->
      A
  end;
read_timer(_) ->
  0.

%% 判断列表中是否有重复项
is_duplicate(List) ->
  is_duplicate(List, []).

is_duplicate([], _) ->
  false;
is_duplicate([H|T], List) ->
  case lists:member(H, List) of
    true ->
      true;
    false ->
      is_duplicate(T, [H|List])
  end.

%% 将列表复制N分，并组合成一个新的列表
copy_list(List,N) ->
  copy_list(List,N,[]).

copy_list(_List, 0, Result) ->
  Result;
copy_list(List, N, Result) ->
  copy_list(List, N-1, List++Result).

%% 找到tupleList中某字段最大的tuple
%% @return: {MaxKey, Tuple}|false
keymax([], _Pos)->
  false;
keymax(List, Pos) ->
  [Head|Tail] = List,
  HeadKey = element(Pos, Head),
  lists:foldl(fun(E, {MaxKey, Tuple}) ->
    Key = element(Pos, E),
    if Key > MaxKey ->
      {Key, E};
      true ->
        {MaxKey, Tuple}
    end
              end, {HeadKey, Head}, Tail).

%% 找到tupleList中某字段最小的tuple
%% @return: {MinKey, Tuple}|false
keymin([], _Pos) ->
  false;
keymin(List, Pos) ->
  [Head|Tail] = List,
  HeadKey = element(Pos, Head),
  lists:foldl(fun(E, {MaxKey, Tuple}) ->
    Key = element(Pos, E),
    if Key < MaxKey ->
      {Key, E};
      true ->
        {MaxKey, Tuple}
    end
              end, {HeadKey, Head}, Tail).

%% 删除第N个，并返回新列表
%% return: {value, NthVar, NewList} | false
nth_take(N, List) ->
  nth_take(N, List, []).
nth_take(1, [NthVar|Tail], Temp) ->
  {value, NthVar, lists:reverse(Temp, Tail)};
nth_take(_N, [], _Temp) ->
  false;
nth_take(N, [Hd | Tail], Temp) ->
  nth_take(N-1, Tail, [Hd|Temp]).

%%检查元素在列表的哪个位置，不在列表中范围列表长度加1
index_in_list(List, Elem)when is_list(List)->
  case util:foldl(fun(AccID, AccN)->
    case AccID=:=Elem of
      true->
        {return, {ok, AccN+1}};
      false->
        AccN+1
    end
                  end, 0, List) of
    {ok, Nth}->
      Nth;
    _nth->
      0
  end.

%% 测试
tc(F, N) when N>0 ->
  Time1 =erlang:monotonic_time(),
  do_times(N, F),
  Time2 = erlang:monotonic_time(),
  MicroDiffPerTime =erlang:convert_time_unit(Time2 - Time1, native, micro_seconds) / N,
  io:format("Times :~w ,  Each time consume: ~w us\n", [N, MicroDiffPerTime]).

do_times(N,_F) when N =< 0 ->ok;
do_times(N,F) when is_function(F, 1)->
  F(N),
  do_times(N -1, F);
do_times(N,F) ->
  F(),
  do_times(N -1, F).

%% 遍历ets的所有key
ets_foreach_key(Fun, Table) ->
  ets:safe_fixtable(Table, true),
  First = ets:first(Table),
  try
    do_ets_foreach_key(Fun, First, Table)
  after
    ets:safe_fixtable(Table, false)
  end.

do_ets_foreach_key(F, Key, Table) ->
  case Key of
    '$end_of_table' ->
      ok;
    _ ->
      F(Key),
      do_ets_foreach_key(F, ets:next(Table, Key), Table)
  end.
%% 获取ets所有key
ets_all_key(Table) ->
  ets:safe_fixtable(Table, true),
  First = ets:first(Table),
  try
    do_ets_all_key(First, Table, [])
  after
    ets:safe_fixtable(Table, false)
  end.

do_ets_all_key(Key, Table, Result) ->
  case Key of
    '$end_of_table' ->
      Result;
    _ ->
      do_ets_all_key(ets:next(Table, Key), Table, [Key|Result])
  end.

%% 计算源码行数
count_src_line_num(Dir) ->
  count_src_line_num(Dir, [".+data_.+\.erl"]).
count_src_line_num(Dir, DisCardRegularExpList) ->
  count_src_line_num(Dir, DisCardRegularExpList, ".+\.erl$").
count_src_line_num(Dir, DisCardRegularExpList, MatchExp) ->
  {TotalNum, Info} =
    filelib:fold_files(Dir, MatchExp, true, fun(FileName, {LineAcc, InfoList}) ->
      case lists:any(fun(E) ->
        case re:run(FileName, E) of
          {match, _} ->
            true;
          _ ->
            false
        end
                     end, DisCardRegularExpList) of
        true ->
          {LineAcc, InfoList};
        false ->
          ThisFileLineNum = cacl_file_line(FileName),
          %io:format("~1000p ------ ~w\n",[filename:basename(FileName), ThisFileLineNum]),
          {LineAcc + ThisFileLineNum, [{ThisFileLineNum, FileName} |InfoList]}
      end
                                            end, {0, []}),
  Info2 = lists:keysort(1, Info),
  lists:foreach(fun({Lines, Name}) ->
    io:format("~1000p ------ ~w\n",[filename:basename(Name), Lines])
                end, Info2),
  io:format("TotalNum=~w\n",[TotalNum]).


cacl_file_line(FileName) ->
  {ok, Bin} = file:read_file(FileName),
  cacl_file_line2(binary_to_list(Bin),0, false).
cacl_file_line2([], Acc, true) ->
  Acc+1;
cacl_file_line2([], Acc, false) ->
  Acc;
cacl_file_line2([$\n|Rest], Acc, true) ->
  cacl_file_line2(Rest, Acc+1, false);
cacl_file_line2([$\n|Rest], Acc, false) ->
  cacl_file_line2(Rest, Acc, false);
cacl_file_line2([_E|Rest], Acc, true) ->
  cacl_file_line2(Rest, Acc, true);
cacl_file_line2([$\r|Rest], Acc, false) ->
  cacl_file_line2(Rest, Acc, false);
cacl_file_line2([$\ |Rest], Acc, false) ->
  cacl_file_line2(Rest, Acc, false);
cacl_file_line2([_|Rest], Acc, false) ->
  cacl_file_line2(Rest, Acc, true).

%% @doc convert IP(tuple) to string()
ip_to_str(IP) ->
  case IP of
    {A, B, C, D} ->
      lists:concat([A, ".", B, ".", C, ".", D]);
    {A, B, C, D, E, F, G, H} ->
      lists:concat([A, ":", B, ":", C, ":", D, ":", E, ":", F, ":", G, ":", H]);
    Str when is_list(Str) ->
      Str;
    _ ->
      []
  end.

%% @doc 简单的mapreduce,仅供脚本使用
upmap(F, L) ->
  Parent = self(),
  Ref = make_ref(),
  [receive
     {Ref,  Result} ->
       Result
   end || _ <- [spawn(fun() -> Parent ! {Ref, F(X)} end)
    || X<-L]].


latin1(Name) ->
  Name2 =
    if is_binary(Name) ->
      binary_to_list(Name);
      true ->
        Name
    end,
  Name3 =
    case is_name_en(Name2) of
      true ->
        Name2;
      false ->
        unicode:characters_to_binary(Name2)
    end,
  Name3.

is_name_en(Name)->
  lists:all(fun(E)->
    if E =< 255->
      true;
      true ->
        false
    end
            end, Name).

words_filter(Words_for_filter) ->
  Words_List = data_words:get(words_list),
  binary:bin_to_list(lists:foldl(fun(Kword, Words_for_filter0)->
    re:replace(Words_for_filter0,Kword,"*",[global,caseless,{return, binary}])
                                 end,
    Words_for_filter,Words_List)).


%% 输入的list为已经解码的utf8 list，检查字符串，只能为数字、大小写英文字母和汉字
check_blankName([]) ->
  true;
check_blankName([H|T]) ->
  case (H >=48 andalso H =< 57) orelse (H >=65 andalso H =< 90) orelse (H >=97 andalso H =< 122) orelse (H >= 16#4e00 andalso H =< 16#9fa5) of
    false ->
      false;
    true ->
      check_blankName(T)
  end.

%% 输入的list为已经解码的utf8 list，计算长度，汉字长度为2
calc_name_length(DecodeList) ->
  lists:foldr(fun(Val, Acc) ->
    case Val >= 16#4e00 andalso Val =< 16#9fa5 of
      true ->
        Acc + 2;
      false ->
        Acc + 1
    end
              end, 0, DecodeList).


%% 将utf8编码的二进制数据解码成字符编码list
gen_utf8_decode_list(<<>>, AccDecodeList) ->
  lists:reverse(AccDecodeList);
gen_utf8_decode_list(<<0:1,X1:1,X2:1,X3:1,X4:1,X5:1,X6:1,X7:1,
  Left/binary>>, AccDecodeList) ->
  Val = get_val([X1, X2, X3, X4, X5, X6, X7]),
  gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(<<1:1,1:1,0:1,X1:1,X2:1,X3:1,X4:1,X5:1,
  1:1,0:1,X6:1,X7:1,X8:1,X9:1,X10:1,X11:1,
  Left/binary>>, AccDecodeList) ->
  Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11]),
  gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(<<1:1,1:1,1:1,0:1,X1:1,X2:1,X3:1,X4:1,
  1:1,0:1,X5:1,X6:1,X7:1,X8:1,X9:1,X10:1,
  1:1,0:1,X11:1,X12:1,X13:1,X14:1,X15:1,X16:1,
  Left/binary>>, AccDecodeList) ->
  Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16]),
  gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(<<1:1,1:1,1:1,1:1,0:1,X1:1,X2:1,X3:1,
  1:1,0:1,X4:1,X5:1,X6:1,X7:1,X8:1,X9:1,
  1:1,0:1,X10:1,X11:1,X12:1,X13:1,X14:1,X15:1,
  1:1,0:1,X16:1,X17:1,X18:1,X19:1,X20:1,X21:1,
  Left/binary>>, AccDecodeList) ->
  Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21]),
  gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(<<1:1,1:1,1:1,1:1,1:1,0:1,X1:1,X2:1,
  1:1,0:1,X3:1,X4:1,X5:1,X6:1,X7:1,X8:1,
  1:1,0:1,X9:1,X10:1,X11:1,X12:1,X13:1,X14:1,
  1:1,0:1,X15:1,X16:1,X17:1,X18:1,X19:1,X20:1,
  1:1,0:1,X21:1,X22:1,X23:1,X24:1,X25:1,X26:1,
  Left/binary>>, AccDecodeList) ->
  Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26]),
  gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(<<1:1,1:1,1:1,1:1,1:1,1:1,0:1,X1:1,
  1:1,0:1,X2:1,X3:1,X4:1,X5:1,X6:1,X7:1,
  1:1,0:1,X8:1,X9:1,X10:1,X11:1,X12:1,X13:1,
  1:1,0:1,X14:1,X15:1,X16:1,X17:1,X18:1,X19:1,
  1:1,0:1,X20:1,X21:1,X22:1,X23:1,X24:1,X25:1,
  1:1,0:1,X26:1,X27:1,X28:1,X29:1,X30:1,X31:1,
  Left/binary>>, AccDecodeList) ->
  Val = get_val([X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, X31]),
  gen_utf8_decode_list(Left, [Val|AccDecodeList]);
gen_utf8_decode_list(Binary, AccDecodeList) ->
%%  ?ERR("非法的utf8编码，Binary：~w, AccDecodeList：~w", [Binary, AccDecodeList]),
  [].

%% 生成单个字符的编码
get_val(List) ->
  {_, Val} =
    lists:foldr(fun(Elem, {Count, AccVal}) ->
      {Count + 1, AccVal + math:pow(2, Count) * Elem}
                end, {0, 0}, List),
  erlang:trunc(Val).

calc_server_id(familyID, FamilyID) ->
  FamilyID div 1000000 - 1;

calc_server_id(roleID, RoleID) ->
  RoleID div 1000000 - 1.



date_to_string(DateTime)->
  {{Y,M,D},{HH,MM,SS}} = DateTime,
  lists:flatten( io_lib:format("~w-~w-~w ~w:~w:~w",[Y,M,D,HH,MM,SS]) ).

time_to_string(Time)->
  {HH,MM,SS} = Time,
  lists:flatten( io_lib:format("~w:~w:~w",[HH,MM,SS]) ).


%% @doc 获取Record的所有值的列表
get_record_values(Record)->
  [_H | Values] = tuple_to_list(Record),
  Values.

-define(STRUCT,struct).
to_jsonencode(V) -> json_encode(prepare_for_json(V)).

prepare_for_json(Int) when is_integer(Int) -> Int;
prepare_for_json(Float) when is_float(Float) -> Float;
prepare_for_json(Atom) when is_atom(Atom) -> Atom;
prepare_for_json(Array) when is_list(Array) ->
  %% case io_lib:printable_list(Array) of
  case io_lib:char_list(Array) of
    true ->
      erlang:list_to_binary(Array);
    false ->
      list_to_json(Array, [])
  end;
prepare_for_json(Tuple) when is_tuple(Tuple) ->
  tuple_to_json(Tuple, erlang:size(Tuple), []);
prepare_for_json(V) -> V.

list_to_json([], Acc) -> lists:reverse(Acc);
list_to_json([{_Key, _Value}|_Rest] = List, Acc) -> {?STRUCT, proplist_to_json(List, Acc)};
list_to_json([H|Rest], Acc) -> list_to_json(Rest, [prepare_for_json(H)|Acc]).

proplist_to_json([], Acc) -> lists:reverse(Acc);
proplist_to_json([{Key, Value}|Rest], Acc) ->
  ValidKey    = prepare_for_json(Key),
  ValidValue  = prepare_for_json(Value),
  proplist_to_json(Rest, [{ValidKey, ValidValue}|Acc]).

tuple_to_json(_Tuple, 0, Acc) ->  {?STRUCT, [erlang:list_to_tuple(Acc)]};
tuple_to_json(Tuple, CurrPos, Acc) ->
  Ele = prepare_for_json(element(CurrPos, Tuple)),
  tuple_to_json(Tuple, CurrPos - 1, [Ele|Acc]).

json_encode(Value) -> mochijson2:encode(Value).


%%============================record to json -----end----==============================================
%% @doc [{1,23},{2,4}]=>"1,23;2,4"
int_tuple_list2string_with_semicolon(List)->
  int_tuple_list2string_with_semicolon(List, "").
int_tuple_list2string_with_semicolon([], Result)->Result;
int_tuple_list2string_with_semicolon([Tuple|List], "")->
  S=string:join([integer_to_list(E)||E<-erlang:tuple_to_list(Tuple)], ","),
  int_tuple_list2string_with_semicolon(List, S);
int_tuple_list2string_with_semicolon([Tuple|List], Result)->
  S=string:join([integer_to_list(E)||E<-erlang:tuple_to_list(Tuple)], ","),
  int_tuple_list2string_with_semicolon(List, Result++";"++S).

%% @doc [{1,23},{2,4}]<="1,23;2,4"
string_with_comma_to_int_tuple_list(BitString) when is_bitstring(BitString) ->
  string_with_comma_to_int_tuple_list(binary_to_list(BitString));
string_with_comma_to_int_tuple_list(String)->
  Tokens=string:tokens(String, ";"),
  lists:map(fun(S)->
    List=string_with_comma_to_int_list(S),
    list_to_tuple(List)
            end, Tokens).

int_list_to_string_with_comma(List) when is_list(List) ->
  string:join([integer_to_list(E)||E<-List], ",").

string_with_comma_to_int_list(BitString) when is_bitstring(BitString) ->
  string_with_comma_to_int_list(binary_to_list(BitString));
string_with_comma_to_int_list(String) ->
  [list_to_integer(X)||X<-string:tokens(String, ",")].


%% @doc 列表去重，并保证顺序
unique_list([])-> [];
unique_list([H])-> [H];
unique_list([H,H])-> [H];
unique_list([H,E])-> [H,E];
unique_list([H|List]) -> unique_list(List, [H]).
unique_list([], Result) -> lists:reverse(Result);
unique_list([H|List], Result)->
  case lists:member(H, Result) of
    false -> unique_list(List, [H|Result]);
    true ->  unique_list(List, Result)
  end.

%% @doc 两个列表有任意一个相同元素
lists_has_same_ele(List1, List2) when List1==[] orelse List2==[] ->false;
lists_has_same_ele([Ele|List1], List2)->
  case lists:member(Ele, List2) of
    true -> true;
    false ->
      lists_has_same_ele(List1, List2)
  end.

%% @doc
list_1_in_list_2([], _) -> [];
list_1_in_list_2(_, []) -> [];
list_1_in_list_2(List1, List2) ->
  list_1_in_list_2(List1, List2, []).
list_1_in_list_2([], _List2, []) -> [];
list_1_in_list_2([], _List2, Result) -> lists:reverse(Result);
list_1_in_list_2([H|List1], List2, Result)->
  case lists:member(H, List2) of
    true -> list_1_in_list_2(List1, List2, [H|Result]);
    false -> list_1_in_list_2(List1, List2, Result)
  end.

list_1_not_in_list_2(List1, []) -> List1;
list_1_not_in_list_2(List1, List2) ->
  list_1_not_in_list_2(List1, List2, []).
list_1_not_in_list_2([], _List2, []) -> [];
list_1_not_in_list_2([], _List2, Result) -> lists:reverse(Result);
list_1_not_in_list_2([H|List1], List2, Result)->
  case lists:member(H, List2) of
    false -> list_1_not_in_list_2(List1, List2, [H|Result]);
    true -> list_1_not_in_list_2(List1, List2, Result)
  end.

merge_kv_list(KVL)->
  merge_kv_list(KVL, []).
merge_kv_list([], Result)->Result;
merge_kv_list({K,V},Result)->
  merge_kv_list([{K,V}],Result);
merge_kv_list([{K,V}|KVL], Result) ->
  case lists:keyfind(K, 1, Result) of
    false->
      merge_kv_list(KVL, [{K,V}|Result]);
    {K, V1}->
      merge_kv_list(KVL, lists:keyreplace(K, 1, Result, {K,V+V1}))
  end.

% for_plus(I,F2,F3,F_BODY,Result)-> %for(int i=?;i<??;++i)
% 	if
% 		F2(I)->
% 			case F_BODY(I,Result) of
% 				{break,R}->
% 					R;
% 				{continue,R}->
%					R;
% 		true->
% 		 	Result
% 	end.

%% @doc 获取零点时间戳
get_zero_clock_stamp()->
  get_zero_clock_stamp(date()).
get_zero_clock_stamp(UnixTime) when is_integer(UnixTime)->
  {Date, _}=seconds_to_datetime(UnixTime),
  get_zero_clock_stamp(Date);
get_zero_clock_stamp({Year,Month,Day})->
  datetime_to_seconds({{Year,Month,Day},{0,0,0}}).

%% @doc 返回下个月第一天
%% @return {Y,M,D}
get_next_month_1st_date({Y,M,_D})->get_next_month_1st_date(Y, M).
get_next_month_1st_date(Y, 12)->{Y+1,1,1};
get_next_month_1st_date(Y, M)->{Y,M+1,1}.



-define(OS_LENGTH, 512).

%% @doc解析客户端机型字段值
parse_machine_devicemodel(Machine)->
  parse_machine(Machine, <<"deviceModel">>).
parse_machine_os(Machine)->
  parse_machine(Machine, <<"operatingSystem">>).
parse_machine(Machine, Field)->
  case catch ejson:decode(Machine) of
    {List} when is_list(List) ->
      lists:sublist(to_string(proplists:get_value(Field,List)), ?OS_LENGTH);
    _ ->
      ""
  end.

is_internal_pid(Pid) when is_pid(Pid) ->
  is_internal_pid(erlang:pid_to_list(Pid));
is_internal_pid("<0."++_)->true;
is_internal_pid(Str) when is_list(Str)->false.



splitwith(F,List)->
  splitwith2(F,List,{[],[]}).


splitwith2(_F,[],{TrueList,FalseList})->
  {lists:reverse(TrueList),lists:reverse(FalseList)};
splitwith2(F,[H|List],{TrueList,FalseList})->
  case F(H) of
    true->
      splitwith2(F,List,{[H|TrueList],FalseList});
    false->
      splitwith2(F,List,{TrueList,[H|FalseList]})
  end.


get_xml_node_value(Node,XmlRoot)->
  Node1 = "//" ++ Node ++ "/text()",
  case catch xmerl_xpath:string(Node1, XmlRoot) of
    [{_,_,_,_,Value,_}] ->
      Value;
    _ ->
      ""
  end.

%%@MapList: [#{}]
%%@return: {ok, Map}|false
map_list_find(Key, Val, [Map|MapList])->
  case maps:find(Key, Map) of
    {ok, Val}->
      {ok, Map};
    _error->
      map_list_find(Key, Val, MapList)
  end;
map_list_find(_Key, _Val, [])->
  false.
%%@MapList: [#{}]
%%@return: NewMapList::[#{}]
map_list_replace(Key, Val, MapList, NewMap)->
  map_list_replace1(Key, Val, MapList, NewMap, []).
map_list_replace1(Key, Val, [Map|MapList], NewMap, List1)->
  case maps:find(Key, Map) of
    {ok, Val}->
      lists:reverse(List1)++[NewMap|MapList];
    _error->
      map_list_replace1(Key, Val, MapList, NewMap, [Map|List1])
  end;
map_list_replace1(_Key, _Val, [], _NewMap, List1)->
  lists:reverse(List1).
%%@MapList: [#{}]
%%@return: NewMapList::[#{}]
map_list_store(Key, Val, MapList, NewMap)->
  map_list_store1(Key, Val, MapList, NewMap, []).
map_list_store1(Key, Val, [Map|MapList], NewMap, List1)->
  case maps:find(Key, Map) of
    {ok, Val}->
      lists:reverse(List1)++[NewMap|MapList];
    _error->
      map_list_store1(Key, Val, MapList, NewMap, [Map|List1])
  end;
map_list_store1(_Key, _Val, [], NewMap, List1)->
  lists:reverse([NewMap|List1]).
%%@MapList: [#{}]
%%@return: NewMapList::[#{}]
map_list_delete(Key, Val, MapList)->
  map_list_delete1(Key, Val, MapList, []).
map_list_delete1(Key, Val, [Map|MapList], List1)->
  case maps:find(Key, Map) of
    {ok, Val}->
      lists:reverse(List1)++MapList;
    _error->
      map_list_delete1(Key, Val, MapList, [Map|List1])
  end;
map_list_delete1(_Key, _Val, [], List1)->
  lists:reverse(List1).

%%@MapList: [#{}]
%%@return: true|false
map_list_is_member(Key, Val, [Map|MapList])->
  case maps:find(Key, Map) of
    {ok, Val}->
      true;
    _error->
      map_list_is_member(Key, Val, MapList)
  end;
map_list_is_member(_Key, _Val, [])->
  false.

%%判断是否为指定类型的map结构
is_map(Map, MTag)->
  case erlang:is_map(Map) of
    true->
      case maps:find(m_tag, Map) of
        {ok, MTag}->
          true;
        _->
          false
      end;
    false->
      false
  end.



