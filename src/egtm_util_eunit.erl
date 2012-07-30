%%
%% $Id: $
%%
%% Module:  egtm_util_eunit -- description
%% Created: 07-MAY-2012 17:41
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

%% @doc EGTM EUnit Utilities.
-module (egtm_util_eunit).
-export ([tc_avg/2, perform_speed/1]).
-compile (export_all).

%% @doc Measure average execution time of specified
%% function `Fun' with arity of 0. The number of
%% time-sampling runs is specified by `Count' argument.
%%
%% Example:
%% ```
%% erl> egtm_util_eunit:tc_avg (fun () ->
%%        io:format ("T=~p~n", [now ()]) end, 5).
%% T={1339,133634,737039}
%% T={1339,133634,737079}
%% T={1339,133634,737105}
%% T={1339,133634,737125}
%% T={1339,133634,737147}
%% 21.1875
%% '''
tc_avg (Fun, Count) when is_function (Fun, 0) ->
  tc_avg_internal (Fun, Count, 0).
tc_avg_internal (_, 0, Avg) -> Avg;
tc_avg_internal (Fun, Count, Avg) ->
  {T,_} = timer:tc (Fun),
  NewAvg = case Avg =:= 0 of
    true  -> T;
    false -> (Avg+T)/2
  end,
  tc_avg_internal (Fun, Count-1, NewAvg).

order_loop (Mod, Type, N) ->
  Gvn = "^%EUnit", Subs = ["perf", "e", ""],
  Next = order_fun (Mod, Gvn, Subs, Type),
  order_loop_internal (Mod, Gvn, Next, Type, N).

order_loop_internal (_, _, _, _, 0) -> ok;
order_loop_internal (Mod, Gvn, Subs, Type, N) ->
  case lists:last (Subs) of
    [] -> ok;
    _  ->
      order_loop_internal (Mod, Gvn,
        order_fun (Mod, Gvn, Subs, Type), Type, N-1)
  end.

order_fun (Mod, Gvn, Subs, erl_xecute) ->
  Mod:xecute_fast_order (Gvn, Subs);
order_fun (Mod, Gvn, Subs, call_xecute) ->
  Mod:call_fast_order (Gvn, Subs);
order_fun (Mod, Gvn, Subs, xecute) ->
  Mod:fast_order (Gvn, Subs);
order_fun (Mod, Gvn, Subs, indirection) ->
  Mod:order (Gvn, Subs).

-ifndef (EUNIT_HRL).
-define (debugMsg(S), (begin
    .io:format ("~s:~w:~w: ~s\n", [?FILE, ?LINE, self (), S]),
    ok
  end)).
-define (debugFmt(S, As), (?debugMsg (.io_lib:format ((S), (As))))).
-endif.

%% @doc Performance testing: runs a tc_avg (Fun, Cnt) multiple times.
%% A typical use is to benchmark multiple functions and compare
%% their average results on single screen.
%%
%% A code of egtm:order_speed_test () unit test might be a good
%% use-case example:
%% ```
%% order_speed_test () ->
%%   egtm:start (),
%%   egtm:do ("testPerfPrepare^%egtmapi()"),
%%
%%   egtm_util_eunit:perform_speed ([
%%     {fun () -> egtm:zversion () end,
%%       50, "Erlang $ZVersion"},
%%     {fun () -> egtm:call ("testPerfOrder1^%egtmapi()") end,
%%       10, "Native $Order"},
%%     {fun () -> egtm:call ("testPerfOrder2^%egtmapi()") end,
%%       10, "Indirection-based"},
%%     {fun () -> egtm:call ("testPerfOrder3^%egtmapi()") end,
%%       10, "Xecute-based"},
%%     {fun () -> egtm_util_eunit:order_loop (egtm, indirection, 100) end,
%%       5, "Erlang Indirection-based"},
%%     {fun () -> egtm_util_eunit:order_loop (egtm, call_xecute, 100) end,
%%       5, "Erlang Call-Xecute-based"},
%%     {fun () -> egtm_util_eunit:order_loop (egtm, xecute, 100) end,
%%       5, "Erlang Xecute-based"},
%%     {fun () -> egtm_util_eunit:order_loop (egtm, erl_xecute, 100) end,
%%       5, "Erlang ErlXecute-based"}
%%   ]),
%%
%%   egtm:stop (),
%%   ok.
%% '''
perform_speed ([]) -> ok;
perform_speed ([{Fun, Count, Label}|T]) when is_function (Fun, 0) ->
  R = egtm_util_eunit:tc_avg (Fun, Count),
  ?debugMsg (?debugFmt ("~n %%%%% Erlang ~s : ~p",
      [string:left (Label, 30), R])),
  perform_speed (T).

rnd_str_fun () ->
  fun (N) -> [$A+random:uniform (26) || _ <- lists:seq (0,N)] end.

%% vim: fdm=syntax:fdn=3:tw=74:ts=2:syn=erlang
