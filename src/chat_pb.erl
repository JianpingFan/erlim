%% -*- coding: utf-8 -*-
%% @private
%% Automatically generated, do not edit
%% Generated by gpb_compile version 4.8.0
-module(chat_pb).

-export([encode_msg/1, encode_msg/2, encode_msg/3]).
-export([decode_msg/2, decode_msg/3]).
-export([merge_msgs/2, merge_msgs/3, merge_msgs/4]).
-export([verify_msg/1, verify_msg/2, verify_msg/3]).
-export([get_msg_defs/0]).
-export([get_msg_names/0]).
-export([get_group_names/0]).
-export([get_msg_or_group_names/0]).
-export([get_enum_names/0]).
-export([find_msg_def/1, fetch_msg_def/1]).
-export([find_enum_def/1, fetch_enum_def/1]).
-export([enum_symbol_by_value/2, enum_value_by_symbol/2]).
-export([get_service_names/0]).
-export([get_service_def/1]).
-export([get_rpc_names/1]).
-export([find_rpc_def/2, fetch_rpc_def/2]).
-export([fqbin_to_service_name/1]).
-export([service_name_to_fqbin/1]).
-export([fqbins_to_service_and_rpc_name/2]).
-export([service_and_rpc_name_to_fqbins/2]).
-export([fqbin_to_msg_name/1]).
-export([msg_name_to_fqbin/1]).
-export([fqbin_to_enum_name/1]).
-export([enum_name_to_fqbin/1]).
-export([get_package_name/0]).
-export([uses_packages/0]).
-export([source_basename/0]).
-export([get_all_source_basenames/0]).
-export([get_all_proto_names/0]).
-export([get_msg_containment/1]).
-export([get_pkg_containment/1]).
-export([get_service_containment/1]).
-export([get_rpc_containment/1]).
-export([get_enum_containment/1]).
-export([get_proto_by_msg_name_as_fqbin/1]).
-export([get_proto_by_service_name_as_fqbin/1]).
-export([get_proto_by_enum_name_as_fqbin/1]).
-export([get_protos_by_pkg_name_as_fqbin/1]).
-export([gpb_version_as_string/0, gpb_version_as_list/0]).

-include("chat_pb.hrl").
-include("gpb.hrl").

%% enumerated types

-export_type([]).

%% message types
-type person() :: #person{}.

-export_type(['person'/0]).

-spec encode_msg(#person{}) -> binary().
encode_msg(Msg) when tuple_size(Msg) >= 1 ->
    encode_msg(Msg, element(1, Msg), []).

-spec encode_msg(#person{}, atom() | list()) -> binary().
encode_msg(Msg, MsgName) when is_atom(MsgName) ->
    encode_msg(Msg, MsgName, []);
encode_msg(Msg, Opts)
    when tuple_size(Msg) >= 1, is_list(Opts) ->
    encode_msg(Msg, element(1, Msg), Opts).

-spec encode_msg(#person{}, atom(), list()) -> binary().
encode_msg(Msg, MsgName, Opts) ->
    case proplists:get_bool(verify, Opts) of
      true -> verify_msg(Msg, MsgName, Opts);
      false -> ok
    end,
    TrUserData = proplists:get_value(user_data, Opts),
    case MsgName of
      person ->
	  encode_msg_person(id(Msg, TrUserData), TrUserData)
    end.


encode_msg_person(Msg, TrUserData) ->
    encode_msg_person(Msg, <<>>, TrUserData).


encode_msg_person(#person{age = F1, name = F2}, Bin,
		  TrUserData) ->
    B1 = begin
	   TrF1 = id(F1, TrUserData),
	   e_type_int32(TrF1, <<Bin/binary, 8>>, TrUserData)
	 end,
    begin
      TrF2 = id(F2, TrUserData),
      e_type_string(TrF2, <<B1/binary, 18>>, TrUserData)
    end.

-compile({nowarn_unused_function,e_type_sint/3}).
e_type_sint(Value, Bin, _TrUserData) when Value >= 0 ->
    e_varint(Value * 2, Bin);
e_type_sint(Value, Bin, _TrUserData) ->
    e_varint(Value * -2 - 1, Bin).

-compile({nowarn_unused_function,e_type_int32/3}).
e_type_int32(Value, Bin, _TrUserData)
    when 0 =< Value, Value =< 127 ->
    <<Bin/binary, Value>>;
e_type_int32(Value, Bin, _TrUserData) ->
    <<N:64/unsigned-native>> = <<Value:64/signed-native>>,
    e_varint(N, Bin).

-compile({nowarn_unused_function,e_type_int64/3}).
e_type_int64(Value, Bin, _TrUserData)
    when 0 =< Value, Value =< 127 ->
    <<Bin/binary, Value>>;
e_type_int64(Value, Bin, _TrUserData) ->
    <<N:64/unsigned-native>> = <<Value:64/signed-native>>,
    e_varint(N, Bin).

-compile({nowarn_unused_function,e_type_bool/3}).
e_type_bool(true, Bin, _TrUserData) ->
    <<Bin/binary, 1>>;
e_type_bool(false, Bin, _TrUserData) ->
    <<Bin/binary, 0>>;
e_type_bool(1, Bin, _TrUserData) -> <<Bin/binary, 1>>;
e_type_bool(0, Bin, _TrUserData) -> <<Bin/binary, 0>>.

-compile({nowarn_unused_function,e_type_string/3}).
e_type_string(S, Bin, _TrUserData) ->
    Utf8 = unicode:characters_to_binary(S),
    Bin2 = e_varint(byte_size(Utf8), Bin),
    <<Bin2/binary, Utf8/binary>>.

-compile({nowarn_unused_function,e_type_bytes/3}).
e_type_bytes(Bytes, Bin, _TrUserData)
    when is_binary(Bytes) ->
    Bin2 = e_varint(byte_size(Bytes), Bin),
    <<Bin2/binary, Bytes/binary>>;
e_type_bytes(Bytes, Bin, _TrUserData)
    when is_list(Bytes) ->
    BytesBin = iolist_to_binary(Bytes),
    Bin2 = e_varint(byte_size(BytesBin), Bin),
    <<Bin2/binary, BytesBin/binary>>.

-compile({nowarn_unused_function,e_type_fixed32/3}).
e_type_fixed32(Value, Bin, _TrUserData) ->
    <<Bin/binary, Value:32/little>>.

-compile({nowarn_unused_function,e_type_sfixed32/3}).
e_type_sfixed32(Value, Bin, _TrUserData) ->
    <<Bin/binary, Value:32/little-signed>>.

-compile({nowarn_unused_function,e_type_fixed64/3}).
e_type_fixed64(Value, Bin, _TrUserData) ->
    <<Bin/binary, Value:64/little>>.

-compile({nowarn_unused_function,e_type_sfixed64/3}).
e_type_sfixed64(Value, Bin, _TrUserData) ->
    <<Bin/binary, Value:64/little-signed>>.

-compile({nowarn_unused_function,e_type_float/3}).
e_type_float(V, Bin, _) when is_number(V) ->
    <<Bin/binary, V:32/little-float>>;
e_type_float(infinity, Bin, _) ->
    <<Bin/binary, 0:16, 128, 127>>;
e_type_float('-infinity', Bin, _) ->
    <<Bin/binary, 0:16, 128, 255>>;
e_type_float(nan, Bin, _) ->
    <<Bin/binary, 0:16, 192, 127>>.

-compile({nowarn_unused_function,e_type_double/3}).
e_type_double(V, Bin, _) when is_number(V) ->
    <<Bin/binary, V:64/little-float>>;
e_type_double(infinity, Bin, _) ->
    <<Bin/binary, 0:48, 240, 127>>;
e_type_double('-infinity', Bin, _) ->
    <<Bin/binary, 0:48, 240, 255>>;
e_type_double(nan, Bin, _) ->
    <<Bin/binary, 0:48, 248, 127>>.

-compile({nowarn_unused_function,e_varint/3}).
e_varint(N, Bin, _TrUserData) -> e_varint(N, Bin).

-compile({nowarn_unused_function,e_varint/2}).
e_varint(N, Bin) when N =< 127 -> <<Bin/binary, N>>;
e_varint(N, Bin) ->
    Bin2 = <<Bin/binary, (N band 127 bor 128)>>,
    e_varint(N bsr 7, Bin2).


decode_msg(Bin, MsgName) when is_binary(Bin) ->
    decode_msg(Bin, MsgName, []).

decode_msg(Bin, MsgName, Opts) when is_binary(Bin) ->
    TrUserData = proplists:get_value(user_data, Opts),
    decode_msg_1_catch(Bin, MsgName, TrUserData).

-ifdef('OTP_RELEASE').
decode_msg_1_catch(Bin, MsgName, TrUserData) ->
    try decode_msg_2_doit(MsgName, Bin, TrUserData)
    catch Class:Reason:StackTrace -> error({gpb_error,{decoding_failure, {Bin, MsgName, {Class, Reason, StackTrace}}}})
    end.
-else.
decode_msg_1_catch(Bin, MsgName, TrUserData) ->
    try decode_msg_2_doit(MsgName, Bin, TrUserData)
    catch Class:Reason ->
        StackTrace = erlang:get_stacktrace(),
        error({gpb_error,{decoding_failure, {Bin, MsgName, {Class, Reason, StackTrace}}}})
    end.
-endif.

decode_msg_2_doit(person, Bin, TrUserData) ->
    id(decode_msg_person(Bin, TrUserData), TrUserData).



decode_msg_person(Bin, TrUserData) ->
    dfp_read_field_def_person(Bin, 0, 0,
			      id(undefined, TrUserData),
			      id(undefined, TrUserData), TrUserData).

dfp_read_field_def_person(<<8, Rest/binary>>, Z1, Z2,
			  F@_1, F@_2, TrUserData) ->
    d_field_person_age(Rest, Z1, Z2, F@_1, F@_2,
		       TrUserData);
dfp_read_field_def_person(<<18, Rest/binary>>, Z1, Z2,
			  F@_1, F@_2, TrUserData) ->
    d_field_person_name(Rest, Z1, Z2, F@_1, F@_2,
			TrUserData);
dfp_read_field_def_person(<<>>, 0, 0, F@_1, F@_2, _) ->
    #person{age = F@_1, name = F@_2};
dfp_read_field_def_person(Other, Z1, Z2, F@_1, F@_2,
			  TrUserData) ->
    dg_read_field_def_person(Other, Z1, Z2, F@_1, F@_2,
			     TrUserData).

dg_read_field_def_person(<<1:1, X:7, Rest/binary>>, N,
			 Acc, F@_1, F@_2, TrUserData)
    when N < 32 - 7 ->
    dg_read_field_def_person(Rest, N + 7, X bsl N + Acc,
			     F@_1, F@_2, TrUserData);
dg_read_field_def_person(<<0:1, X:7, Rest/binary>>, N,
			 Acc, F@_1, F@_2, TrUserData) ->
    Key = X bsl N + Acc,
    case Key of
      8 ->
	  d_field_person_age(Rest, 0, 0, F@_1, F@_2, TrUserData);
      18 ->
	  d_field_person_name(Rest, 0, 0, F@_1, F@_2, TrUserData);
      _ ->
	  case Key band 7 of
	    0 ->
		skip_varint_person(Rest, 0, 0, F@_1, F@_2, TrUserData);
	    1 -> skip_64_person(Rest, 0, 0, F@_1, F@_2, TrUserData);
	    2 ->
		skip_length_delimited_person(Rest, 0, 0, F@_1, F@_2,
					     TrUserData);
	    3 ->
		skip_group_person(Rest, Key bsr 3, 0, F@_1, F@_2,
				  TrUserData);
	    5 -> skip_32_person(Rest, 0, 0, F@_1, F@_2, TrUserData)
	  end
    end;
dg_read_field_def_person(<<>>, 0, 0, F@_1, F@_2, _) ->
    #person{age = F@_1, name = F@_2}.

d_field_person_age(<<1:1, X:7, Rest/binary>>, N, Acc,
		   F@_1, F@_2, TrUserData)
    when N < 57 ->
    d_field_person_age(Rest, N + 7, X bsl N + Acc, F@_1,
		       F@_2, TrUserData);
d_field_person_age(<<0:1, X:7, Rest/binary>>, N, Acc, _,
		   F@_2, TrUserData) ->
    {NewFValue, RestF} = {begin
			    <<Res:32/signed-native>> = <<(X bsl N +
							    Acc):32/unsigned-native>>,
			    id(Res, TrUserData)
			  end,
			  Rest},
    dfp_read_field_def_person(RestF, 0, 0, NewFValue, F@_2,
			      TrUserData).

d_field_person_name(<<1:1, X:7, Rest/binary>>, N, Acc,
		    F@_1, F@_2, TrUserData)
    when N < 57 ->
    d_field_person_name(Rest, N + 7, X bsl N + Acc, F@_1,
			F@_2, TrUserData);
d_field_person_name(<<0:1, X:7, Rest/binary>>, N, Acc,
		    F@_1, _, TrUserData) ->
    {NewFValue, RestF} = begin
			   Len = X bsl N + Acc,
			   <<Bytes:Len/binary, Rest2/binary>> = Rest,
			   {id(binary:copy(Bytes), TrUserData), Rest2}
			 end,
    dfp_read_field_def_person(RestF, 0, 0, F@_1, NewFValue,
			      TrUserData).

skip_varint_person(<<1:1, _:7, Rest/binary>>, Z1, Z2,
		   F@_1, F@_2, TrUserData) ->
    skip_varint_person(Rest, Z1, Z2, F@_1, F@_2,
		       TrUserData);
skip_varint_person(<<0:1, _:7, Rest/binary>>, Z1, Z2,
		   F@_1, F@_2, TrUserData) ->
    dfp_read_field_def_person(Rest, Z1, Z2, F@_1, F@_2,
			      TrUserData).

skip_length_delimited_person(<<1:1, X:7, Rest/binary>>,
			     N, Acc, F@_1, F@_2, TrUserData)
    when N < 57 ->
    skip_length_delimited_person(Rest, N + 7, X bsl N + Acc,
				 F@_1, F@_2, TrUserData);
skip_length_delimited_person(<<0:1, X:7, Rest/binary>>,
			     N, Acc, F@_1, F@_2, TrUserData) ->
    Length = X bsl N + Acc,
    <<_:Length/binary, Rest2/binary>> = Rest,
    dfp_read_field_def_person(Rest2, 0, 0, F@_1, F@_2,
			      TrUserData).

skip_group_person(Bin, FNum, Z2, F@_1, F@_2,
		  TrUserData) ->
    {_, Rest} = read_group(Bin, FNum),
    dfp_read_field_def_person(Rest, 0, Z2, F@_1, F@_2,
			      TrUserData).

skip_32_person(<<_:32, Rest/binary>>, Z1, Z2, F@_1,
	       F@_2, TrUserData) ->
    dfp_read_field_def_person(Rest, Z1, Z2, F@_1, F@_2,
			      TrUserData).

skip_64_person(<<_:64, Rest/binary>>, Z1, Z2, F@_1,
	       F@_2, TrUserData) ->
    dfp_read_field_def_person(Rest, Z1, Z2, F@_1, F@_2,
			      TrUserData).

read_group(Bin, FieldNum) ->
    {NumBytes, EndTagLen} = read_gr_b(Bin, 0, 0, 0, 0, FieldNum),
    <<Group:NumBytes/binary, _:EndTagLen/binary, Rest/binary>> = Bin,
    {Group, Rest}.

%% Like skipping over fields, but record the total length,
%% Each field is <(FieldNum bsl 3) bor FieldType> ++ <FieldValue>
%% Record the length because varints may be non-optimally encoded.
%%
%% Groups can be nested, but assume the same FieldNum cannot be nested
%% because group field numbers are shared with the rest of the fields
%% numbers. Thus we can search just for an group-end with the same
%% field number.
%%
%% (The only time the same group field number could occur would
%% be in a nested sub message, but then it would be inside a
%% length-delimited entry, which we skip-read by length.)
read_gr_b(<<1:1, X:7, Tl/binary>>, N, Acc, NumBytes, TagLen, FieldNum)
  when N < (32-7) ->
    read_gr_b(Tl, N+7, X bsl N + Acc, NumBytes, TagLen+1, FieldNum);
read_gr_b(<<0:1, X:7, Tl/binary>>, N, Acc, NumBytes, TagLen,
          FieldNum) ->
    Key = X bsl N + Acc,
    TagLen1 = TagLen + 1,
    case {Key bsr 3, Key band 7} of
        {FieldNum, 4} -> % 4 = group_end
            {NumBytes, TagLen1};
        {_, 0} -> % 0 = varint
            read_gr_vi(Tl, 0, NumBytes + TagLen1, FieldNum);
        {_, 1} -> % 1 = bits64
            <<_:64, Tl2/binary>> = Tl,
            read_gr_b(Tl2, 0, 0, NumBytes + TagLen1 + 8, 0, FieldNum);
        {_, 2} -> % 2 = length_delimited
            read_gr_ld(Tl, 0, 0, NumBytes + TagLen1, FieldNum);
        {_, 3} -> % 3 = group_start
            read_gr_b(Tl, 0, 0, NumBytes + TagLen1, 0, FieldNum);
        {_, 4} -> % 4 = group_end
            read_gr_b(Tl, 0, 0, NumBytes + TagLen1, 0, FieldNum);
        {_, 5} -> % 5 = bits32
            <<_:32, Tl2/binary>> = Tl,
            read_gr_b(Tl2, 0, 0, NumBytes + TagLen1 + 4, 0, FieldNum)
    end.

read_gr_vi(<<1:1, _:7, Tl/binary>>, N, NumBytes, FieldNum)
  when N < (64-7) ->
    read_gr_vi(Tl, N+7, NumBytes+1, FieldNum);
read_gr_vi(<<0:1, _:7, Tl/binary>>, _, NumBytes, FieldNum) ->
    read_gr_b(Tl, 0, 0, NumBytes+1, 0, FieldNum).

read_gr_ld(<<1:1, X:7, Tl/binary>>, N, Acc, NumBytes, FieldNum)
  when N < (64-7) ->
    read_gr_ld(Tl, N+7, X bsl N + Acc, NumBytes+1, FieldNum);
read_gr_ld(<<0:1, X:7, Tl/binary>>, N, Acc, NumBytes, FieldNum) ->
    Len = X bsl N + Acc,
    NumBytes1 = NumBytes + 1,
    <<_:Len/binary, Tl2/binary>> = Tl,
    read_gr_b(Tl2, 0, 0, NumBytes1 + Len, 0, FieldNum).

merge_msgs(Prev, New)
    when element(1, Prev) =:= element(1, New) ->
    merge_msgs(Prev, New, element(1, Prev), []).

merge_msgs(Prev, New, MsgName) when is_atom(MsgName) ->
    merge_msgs(Prev, New, MsgName, []);
merge_msgs(Prev, New, Opts)
    when element(1, Prev) =:= element(1, New),
	 is_list(Opts) ->
    merge_msgs(Prev, New, element(1, Prev), Opts).

merge_msgs(Prev, New, MsgName, Opts) ->
    TrUserData = proplists:get_value(user_data, Opts),
    case MsgName of
      person -> merge_msg_person(Prev, New, TrUserData)
    end.

-compile({nowarn_unused_function,merge_msg_person/3}).
merge_msg_person(#person{},
		 #person{age = NFage, name = NFname}, _) ->
    #person{age = NFage, name = NFname}.


verify_msg(Msg) when tuple_size(Msg) >= 1 ->
    verify_msg(Msg, element(1, Msg), []);
verify_msg(X) ->
    mk_type_error(not_a_known_message, X, []).

verify_msg(Msg, MsgName) when is_atom(MsgName) ->
    verify_msg(Msg, MsgName, []);
verify_msg(Msg, Opts) when tuple_size(Msg) >= 1 ->
    verify_msg(Msg, element(1, Msg), Opts);
verify_msg(X, _Opts) ->
    mk_type_error(not_a_known_message, X, []).

verify_msg(Msg, MsgName, Opts) ->
    TrUserData = proplists:get_value(user_data, Opts),
    case MsgName of
      person -> v_msg_person(Msg, [MsgName], TrUserData);
      _ -> mk_type_error(not_a_known_message, Msg, [])
    end.


-compile({nowarn_unused_function,v_msg_person/3}).
-dialyzer({nowarn_function,v_msg_person/3}).
v_msg_person(#person{age = F1, name = F2}, Path,
	     TrUserData) ->
    v_type_int32(F1, [age | Path], TrUserData),
    v_type_string(F2, [name | Path], TrUserData),
    ok;
v_msg_person(X, Path, _TrUserData) ->
    mk_type_error({expected_msg, person}, X, Path).

-compile({nowarn_unused_function,v_type_int32/3}).
-dialyzer({nowarn_function,v_type_int32/3}).
v_type_int32(N, _Path, _TrUserData)
    when -2147483648 =< N, N =< 2147483647 ->
    ok;
v_type_int32(N, Path, _TrUserData) when is_integer(N) ->
    mk_type_error({value_out_of_range, int32, signed, 32},
		  N, Path);
v_type_int32(X, Path, _TrUserData) ->
    mk_type_error({bad_integer, int32, signed, 32}, X,
		  Path).

-compile({nowarn_unused_function,v_type_string/3}).
-dialyzer({nowarn_function,v_type_string/3}).
v_type_string(S, Path, _TrUserData)
    when is_list(S); is_binary(S) ->
    try unicode:characters_to_binary(S) of
      B when is_binary(B) -> ok;
      {error, _, _} ->
	  mk_type_error(bad_unicode_string, S, Path)
    catch
      error:badarg ->
	  mk_type_error(bad_unicode_string, S, Path)
    end;
v_type_string(X, Path, _TrUserData) ->
    mk_type_error(bad_unicode_string, X, Path).

-compile({nowarn_unused_function,mk_type_error/3}).
-spec mk_type_error(_, _, list()) -> no_return().
mk_type_error(Error, ValueSeen, Path) ->
    Path2 = prettify_path(Path),
    erlang:error({gpb_type_error,
		  {Error, [{value, ValueSeen}, {path, Path2}]}}).


-compile({nowarn_unused_function,prettify_path/1}).
-dialyzer({nowarn_function,prettify_path/1}).
prettify_path([]) -> top_level;
prettify_path(PathR) ->
    list_to_atom(lists:append(lists:join(".",
					 lists:map(fun atom_to_list/1,
						   lists:reverse(PathR))))).


-compile({nowarn_unused_function,id/2}).
-compile({inline,id/2}).
id(X, _TrUserData) -> X.

-compile({nowarn_unused_function,v_ok/3}).
-compile({inline,v_ok/3}).
v_ok(_Value, _Path, _TrUserData) -> ok.

-compile({nowarn_unused_function,m_overwrite/3}).
-compile({inline,m_overwrite/3}).
m_overwrite(_Prev, New, _TrUserData) -> New.

-compile({nowarn_unused_function,cons/3}).
-compile({inline,cons/3}).
cons(Elem, Acc, _TrUserData) -> [Elem | Acc].

-compile({nowarn_unused_function,lists_reverse/2}).
-compile({inline,lists_reverse/2}).
'lists_reverse'(L, _TrUserData) -> lists:reverse(L).
-compile({nowarn_unused_function,'erlang_++'/3}).
-compile({inline,'erlang_++'/3}).
'erlang_++'(A, B, _TrUserData) -> A ++ B.

get_msg_defs() ->
    [{{msg, person},
      [#field{name = age, fnum = 1, rnum = 2, type = int32,
	      occurrence = required, opts = []},
       #field{name = name, fnum = 2, rnum = 3, type = string,
	      occurrence = required, opts = []}]}].


get_msg_names() -> [person].


get_group_names() -> [].


get_msg_or_group_names() -> [person].


get_enum_names() -> [].


fetch_msg_def(MsgName) ->
    case find_msg_def(MsgName) of
      Fs when is_list(Fs) -> Fs;
      error -> erlang:error({no_such_msg, MsgName})
    end.


-spec fetch_enum_def(_) -> no_return().
fetch_enum_def(EnumName) ->
    erlang:error({no_such_enum, EnumName}).


find_msg_def(person) ->
    [#field{name = age, fnum = 1, rnum = 2, type = int32,
	    occurrence = required, opts = []},
     #field{name = name, fnum = 2, rnum = 3, type = string,
	    occurrence = required, opts = []}];
find_msg_def(_) -> error.


find_enum_def(_) -> error.


-spec enum_symbol_by_value(_, _) -> no_return().
enum_symbol_by_value(E, V) ->
    erlang:error({no_enum_defs, E, V}).


-spec enum_value_by_symbol(_, _) -> no_return().
enum_value_by_symbol(E, V) ->
    erlang:error({no_enum_defs, E, V}).



get_service_names() -> [].


get_service_def(_) -> error.


get_rpc_names(_) -> error.


find_rpc_def(_, _) -> error.



-spec fetch_rpc_def(_, _) -> no_return().
fetch_rpc_def(ServiceName, RpcName) ->
    erlang:error({no_such_rpc, ServiceName, RpcName}).


%% Convert a a fully qualified (ie with package name) service name
%% as a binary to a service name as an atom.
-spec fqbin_to_service_name(_) -> no_return().
fqbin_to_service_name(X) ->
    error({gpb_error, {badservice, X}}).


%% Convert a service name as an atom to a fully qualified
%% (ie with package name) name as a binary.
-spec service_name_to_fqbin(_) -> no_return().
service_name_to_fqbin(X) ->
    error({gpb_error, {badservice, X}}).


%% Convert a a fully qualified (ie with package name) service name
%% and an rpc name, both as binaries to a service name and an rpc
%% name, as atoms.
-spec fqbins_to_service_and_rpc_name(_, _) -> no_return().
fqbins_to_service_and_rpc_name(S, R) ->
    error({gpb_error, {badservice_or_rpc, {S, R}}}).


%% Convert a service name and an rpc name, both as atoms,
%% to a fully qualified (ie with package name) service name and
%% an rpc name as binaries.
-spec service_and_rpc_name_to_fqbins(_, _) -> no_return().
service_and_rpc_name_to_fqbins(S, R) ->
    error({gpb_error, {badservice_or_rpc, {S, R}}}).


fqbin_to_msg_name(<<"person">>) -> person;
fqbin_to_msg_name(E) -> error({gpb_error, {badmsg, E}}).


msg_name_to_fqbin(person) -> <<"person">>;
msg_name_to_fqbin(E) -> error({gpb_error, {badmsg, E}}).


-spec fqbin_to_enum_name(_) -> no_return().
fqbin_to_enum_name(E) ->
    error({gpb_error, {badenum, E}}).


-spec enum_name_to_fqbin(_) -> no_return().
enum_name_to_fqbin(E) ->
    error({gpb_error, {badenum, E}}).


get_package_name() -> undefined.


%% Whether or not the message names
%% are prepended with package name or not.
uses_packages() -> false.


source_basename() -> "chat.proto".


%% Retrieve all proto file names, also imported ones.
%% The order is top-down. The first element is always the main
%% source file. The files are returned with extension,
%% see get_all_proto_names/0 for a version that returns
%% the basenames sans extension
get_all_source_basenames() -> ["chat.proto"].


%% Retrieve all proto file names, also imported ones.
%% The order is top-down. The first element is always the main
%% source file. The files are returned sans .proto extension,
%% to make it easier to use them with the various get_xyz_containment
%% functions.
get_all_proto_names() -> ["chat"].


get_msg_containment("chat") -> [person];
get_msg_containment(P) ->
    error({gpb_error, {badproto, P}}).


get_pkg_containment("chat") -> undefined;
get_pkg_containment(P) ->
    error({gpb_error, {badproto, P}}).


get_service_containment("chat") -> [];
get_service_containment(P) ->
    error({gpb_error, {badproto, P}}).


get_rpc_containment("chat") -> [];
get_rpc_containment(P) ->
    error({gpb_error, {badproto, P}}).


get_enum_containment("chat") -> [];
get_enum_containment(P) ->
    error({gpb_error, {badproto, P}}).


get_proto_by_msg_name_as_fqbin(<<"person">>) -> "chat";
get_proto_by_msg_name_as_fqbin(E) ->
    error({gpb_error, {badmsg, E}}).


-spec get_proto_by_service_name_as_fqbin(_) -> no_return().
get_proto_by_service_name_as_fqbin(E) ->
    error({gpb_error, {badservice, E}}).


-spec get_proto_by_enum_name_as_fqbin(_) -> no_return().
get_proto_by_enum_name_as_fqbin(E) ->
    error({gpb_error, {badenum, E}}).


-spec get_protos_by_pkg_name_as_fqbin(_) -> no_return().
get_protos_by_pkg_name_as_fqbin(E) ->
    error({gpb_error, {badpkg, E}}).



gpb_version_as_string() ->
    "4.8.0".

gpb_version_as_list() ->
    [4,8,0].
