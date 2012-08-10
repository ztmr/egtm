%%
%% $Id: $
%%
%% Module:  egtm_util -- description
%% Created: 07-APR-2012 15:31
%% Author:  tmr
%%
%% Copyright 2012 Tomas Morstein, IDEA Systems.
%%
%% This program is free software: you can redistribute
%% it and/or modify it under the terms of the GNU Affero
%% General Public License as published by the Free Software
%% Foundation, either version 3 of the License,
%% or (at your option) any later version.
%%
%% This program is distributed in the hope that it will
%% be useful, but WITHOUT ANY WARRANTY; without even
%% the implied warranty of MERCHANTABILITY or FITNESS
%% FOR A PARTICULAR PURPOSE. See the GNU Affero General
%% Public License for more details.
%%
%% You should have received a copy of the GNU Affero
%% General Public License along with this program.
%% If not, see <http://www.gnu.org/licenses/>.

%% @doc EGTM Utilities.
-module (egtm_util).
-export ([
  stringify/1,
  gforeach/0, gforeach/1,
  foreach/3, foreach/2,
  set_term/1, transaction/1, lock/2, lock/3,
  longstring_set/4, longstring_set/3,
  longstring_get/2, longstring_kill/2
]).

-include_lib ("egtm.hrl").

%% @doc Convert any datatype to string.
-spec stringify (Data::any ()) -> Result::string ().
stringify (X) when is_list (X) -> X;
stringify (X) -> io_lib:format ("~p", [X]).

%% @doc Sets terminal characteristics. At the moment,
%% only `NOCENABLE' is currently supported. (= `U $P:NOCENABLE').
-spec set_term (Flag::nocenable) -> ok.
set_term ('nocenable') -> egtm:xecute ("u $p:nocenable").

%% @doc Foreach implementation based on `egtm:order()'
%% and `egtm:get()'.
%% `Gvn' is a name of MUMPS global array.
%% `Subs' is a list of keys (subscripts) `[S1,...,SN]
%% so that S(N+1) will be used to `egtm:order()' over.
%% `Fun' argument is function of arity 2 (Gvn, Subs)
%% or arity 3 (Gvn, Subs, ResultAccumulator).
%% If the `Fun' argument is not specified, all the
%% records will be written on standard output via
%% `io:format()'.
%%
%% Example:
%% ```
%% egtm_util:foreach ("^Foo", [1,2,3], fun (G, S) ->
%%   io:format ("~p ~p -> ~p", [G, S, egtm:get (G, S)]) end).
%% '''
%% ...is equivalent of MUMPS code similar to this:
%% ```
%% N X S X="" F  S X=$O(^Foo(1,2,3,X)) Q:X=""  D
%% . W $NA(^(X)),"=",$G(^(X)),!
%% '''
-spec foreach (Gvn::global_name (),
               Subs::subscripts ()) ->
               nomatch | {ok, AccumulatedData::list ()}.
foreach (Gvn, Subs) ->
  foreach (Gvn, Subs, fun (G,S) ->
    io:format ("~s~p=~p~n",
      [G, S, egtm:get (G, S)]), lists:last (S) end).

%% @equiv foreach (Gvn, Subs)
-spec foreach (Gvn::global_name (),
               Subs::subscripts (),
               Fun::function ()) ->
               nomatch | {ok, AccumulatedData::list ()}.
foreach (Gvn, Subs, Fun) when is_function (Fun, 2) ->
  case foreach (Gvn, Subs, fun (G, S, R) -> [Fun (G, S)|R] end) of
    {ok, Res} -> {ok, lists:reverse (Res)};
    Whatever  -> Whatever
  end;
foreach (Gvn, Subs, Fun) when is_function (Fun, 3) ->
  case egtm:data (Gvn, Subs) > 0 of
    true  -> foreach_internal (Gvn, Subs, length (Subs), [], Fun, []);
    false -> nomatch
  end.

foreach_internal (Gvn, SubH, SubHLen, SubT, Fun, Res) ->
  NewSubs = egtm:order (Gvn, SubH++[SubT]),
  NewSubT = lists:nth (SubHLen+1, NewSubs),
  case NewSubT of
    [] ->
      {ok, Res};
    _  ->
      foreach_internal (Gvn, SubH, SubHLen, NewSubT,
                        Fun, Fun (Gvn, NewSubs, Res))
  end.

%% @equiv gforeach (PrintAllGvnsFunction)
gforeach () ->
  gforeach (fun (G) ->
    io:format ("~s=~p~n", [G, egtm:get (G)]), G end).

%% @doc Global Variable Name Foreach.
%% Uses special case of MUMPS `$Order'
%% (GT.M-specific trick!) to iterate over
%% all global variables available.
%%
%% `Fun' is function of arity one or two.
%% First argument passed to `Fun' is the
%% name of global variable and the second
%% (optional) is result accumulator variable.
%%
%% When used with the `Fun' of arity of one,
%% the accumulator is automatically collected
%% as the list of all the results of each
%% `Fun' call.
%%
%% Examples:
%% ```
%% erl> egtm_util:gforeach ().
%% ^%EUnit=[]
%% ^ZFOO="1"
%% ^ZTMR="1"
%% {ok,["^%EUnit","^ZFOO","^ZTMR"]}
%%
%% erl> egtm_util:gforeach (fun (G) -> G end).
%% {ok,["^%EUnit","^ZFOO","^ZTMR"]}
%%
%% erl> egtm_util:gforeach (fun (G) -> egtm:data (G) end).
%% {ok,[10,1,11]}
%%
%% erl> egtm_util:gforeach (fun (G, Res) -> {ok, R} = egtm_util:foreach (G, []), Res end).
%% ^%EUnit["perf"]=[]
%% ^ZTMR["1"]="2"
%% ^ZTMR["2"]="3"
%% ^ZTMR["3"]="4"
%% {ok,[]}
%%
%% erl> egtm_util:gforeach (fun (G, Res) ->
%%        {ok, R} = egtm_util:foreach (G, [],
%%          fun (G, S, A) -> [{S,egtm:get (G, S)}|A] end),
%%        [{G,R}|Res] end).
%% {ok,[{"^ZTMR",[{["3"],"4"},{["2"],"3"},{["1"],"2"}]},
%%      {"^ZFOO",[]},
%%      {"^%EUnit",[{["perf"],[]}]}]}
%% '''
gforeach (Fun) when is_function (Fun, 1) ->
  case gforeach (fun (G, R) -> [Fun (G)|R] end) of
    {ok, Res} -> {ok, lists:reverse (Res)};
    Whatever  -> Whatever
  end;
gforeach (Fun) when is_function (Fun, 2) ->
  gforeach_internal (egtm:gorder (), Fun, []).

gforeach_internal ([], _Fun, Res) -> {ok, Res};
gforeach_internal (Gvn, Fun, Res) ->
  gforeach_internal (egtm:order (Gvn), Fun, Fun (Gvn, Res)).

tp_finish (ok) -> tp_finish (true);
tp_finish (true) -> tp_finish ({ok, done});
tp_finish (false) -> tp_finish ({error, unknown});
tp_finish ({ok, Status}) ->
  egtm:tcommit (), {ok, commit, Status};
tp_finish ({'EXIT', Error}) ->
  tp_finish ({error, {exception, Error}});
tp_finish ({error, Status}) ->
  egtm:trollback (), {ok, rollback, Status};
tp_finish (Status) ->
  egtm:trollback (), {ok, rollback, Status}.

%% @doc Transaction processing (TP) support.
%% The only parameter of `Fun' is function
%% that is to be run within a transaction
%% block.
%%
%% Example:
%% ```
%% case transaction (fun () ->
%%     egtm:lock (Gvn, Subs),
%%     egtm:kill (Gvn, Subs),
%%     egtm:set (Gvn, Subs, Value),
%%     egtm:unlock (Gvn, Subs)
%%   end) of
%%
%%   {ok, commit, Res} -> {ok, Res};
%%   Whatever          -> {error, Whatever}
%% end
%% '''
%% ...is a Erlang equivalent of MUMPS code similar to
%% (if `Gvn="^Foo"', `Subs=[1,2,3]' and `Value="abc"'):
%% ```TS  L +^Foo(1,2,3) K ^Foo(1,2,3) S ^Foo(1,2,3)="abc" L -^Foo(1,2,3) TC'''
-spec transaction (Fun::function ()) ->
  {ok, commit | rollback, Status::any ()}.
transaction (Fun) when is_function (Fun, 0) ->
  egtm:tstart (), tp_finish (catch (Fun ())).

%% @doc Run a function `Fun' within a lock-block
%% on `Gvn' global with `Subs' subscripts.
-spec lock (Gvn::global_name (), Subs::subscripts (),
  Fun::function ()) -> Result::any ().
lock (Gvn, Subs, Fun) when is_function (Fun) ->
  egtm:lock (Gvn, Subs),
  Res = Fun (),
  egtm:unlock (Gvn, Subs),
  Res.

%% @equiv lock (Gvn, Subs, Fun)
-spec lock (Gvn::global_name (), Fun::function ()) -> Result::any ().
lock (Gvn, Fun) when is_function (Fun) ->
  lock (Gvn, [], Fun).

%% @doc Longstring get/set support.
%% `longstring_set' and `longstring_get' usually operates
%% on strings with sizes larger than maximal GT.M string
%% (1MB) limit.
%% The mechanism is based on cutting strings into chunks
%% of (optionally) specified length `BlockSize' and putting
%% them into subindex of `Subs' subscripts in `Gvn' global.
%%
%% `BlockSize' cannot be less than 1 and if specified
%% incorrectly or completely missing, it is expected
%% to be 4000 by default.
%%
%% Example (`BlockSize=5'):
%% ```
%% erl> egtm_util:longstring_set ("^Foo", ["a","b"], "hello world from erlang and gt.m!", 5).
%% {ok,done}
%%
%% erl> egtm_util:longstring_get ("^Foo", ["a","b"]).
%% "hello world from erlang and gt.m!"
%%
%% erl> egtm_util:foreach ("^Foo", ["a","b"]).
%% ^Foo["a","b","1"]="hello"
%% ^Foo["a","b","2"]=" worl"
%% ^Foo["a","b","3"]="d fro"
%% ^Foo["a","b","4"]="m erl"
%% ^Foo["a","b","5"]="ang a"
%% ^Foo["a","b","6"]="nd gt"
%% ^Foo["a","b","7"]=".m!"
%% '''
-spec longstring_set (Gvn::global_name (),
                      Subs::subscripts (),
                      Text::string (),
                      BlockSize::integer ()) ->
  {ok, Result::any ()} | {error, Reason::any ()}.
longstring_set (Gvn, Subs, Text, BlockSize) when is_integer (BlockSize) ->
  BS = (fun (N) when N < 1 -> ?EGTM_LONGSTRING_BLOCKSIZE;
            (N) -> N end)(BlockSize),
  case transaction (fun () ->
      egtm:lock (Gvn, Subs),
      egtm:kill (Gvn, Subs),
      Data = case Text of
        undefined -> []; null -> [];
        Val when is_atom (Val) -> atom_to_list (Val);
        Val when is_binary (Val) -> binary_to_list (Val);
        Val when is_list (Val) -> Val;
        Val -> lists:flatten (io_lib:format ("~p", [Val]))
      end,
      longstring_set_internal (Gvn, Subs, Text,
        length (Data), BS, 1),
      egtm:unlock (Gvn, Subs)
    end) of

    {ok, commit, Res} -> {ok, Res};
    Whatever          -> {error, Whatever}
  end.

%% @equiv longstring_set (Gvn, Subs, Text, 4000)
-spec longstring_set (Gvn::global_name (),
                      Subs::subscripts (),
                      Text::string ()) ->
  {ok, Result::any ()} | {error, Reason::any ()}.
longstring_set (Gvn, Subs, Text) ->
  longstring_set (Gvn, Subs, Text, ?EGTM_LONGSTRING_BLOCKSIZE).

longstring_set_internal (_, _, [], _, _, _) -> ok;
longstring_set_internal (Gvn, Subs, Text, TextLen, BS, N) ->
  {H, T} = case TextLen < BS of
    true  -> {Text, []};
    false -> lists:split (BS, Text)
  end,
  egtm:set (Gvn, Subs++[N], H),
  longstring_set_internal (Gvn, Subs, T, TextLen-BS, BS, N+1).

%% @equiv longstring_set (Gvn, Subs, Text)
-spec longstring_get (Gvn::global_name (),
                      Subs::subscripts ()) ->
                      Result::string ().
longstring_get (Gvn, Subs) ->
  case egtm_util:foreach (Gvn, Subs,
           fun (G,S) -> egtm:get (G,S)
         end) of
    {ok, Res} -> lists:flatten (Res);
    nomatch   -> []
  end.

%% @equiv longstring_set (Gvn, Subs, [])
-spec longstring_kill (Gvn::global_name (),
                       Subs::subscripts ()) ->
  {ok, Result::any ()} | {error, Reason::any ()}.
longstring_kill (Gvn, Subs) ->
  longstring_set (Gvn, Subs, []).

%% EUnit Tests
-ifdef (TEST).
-include_lib ("eunit/include/eunit.hrl").

longstring_test () ->
  egtm:start (),
  RndStr = egtm_util_eunit:rnd_str_fun (),
  {Gvn, Subs} = {"^EUnit", [RndStr (8), "longstring"]},
  TextLong = RndStr (10240), TextShort = RndStr (1024),

  longstring_set (Gvn, Subs, TextLong),
  ?assertEqual (TextLong, longstring_get (Gvn, Subs)),

  longstring_set (Gvn, Subs, TextShort),
  ?assertEqual (TextShort, longstring_get (Gvn, Subs)),

  longstring_set (Gvn, Subs, ""),
  ?assertEqual ("", longstring_get (Gvn, Subs)),
  egtm:stop (),
  ok.

foreach_test () ->
  ok.
-endif.

%% vim: fdm=syntax:fdn=3:tw=74:ts=2:syn=erlang
