%%
%% $Id: $
%%
%% Module:  egtm_util_fmt -- description
%% Created: 29-DEC-2012 23:34
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

-module (egtm_util_fmt).

-export ([
  format_gvn/2, format_gvn/3,
  format_pgm_call/2,
  escape_key/2, unescape_key/1,
  escape_val/1, unescape_val/1,
  term2str/1,
  mumps_escape/1, mumps_unescape/1
]).

-include_lib ("egtm.hrl").

%% @doc Format a MUMPS global name.
%% This function builds a full global name reference
%% composed from `Gvn' variable name and its `Subs'
%  subscripts (indices).
%%
%% ```
%% erl> format_gvn ("^HelloWorld", ["czech","ahoj svete"])
%% "^HelloWorld(\"czech\",\"ahoj svete\")"
%% '''
%%
%% Note that it works for both local and global variables
%% since they differ only in the leading carret.
format_gvn (Gvn, Subs) -> format_gvn (Gvn, Subs, false).

%% @doc A bit more flexible variant of format_gvn/2.
%% The third argument `AllowNull' can be used to change
%% behaviour of last subscript formatting.
%%
%% Even if GT.M does not allow null subscripts by default,
%% the empty subscript on the last position is useful
%% especially when used in conjunction with `$Order' operation.
format_gvn (Gvn, [], _) -> Gvn;
format_gvn (Gvn, Subs, AllowNull) ->
  format_gvn_internal (Gvn, [], Subs, AllowNull).

format_gvn_internal (Gvn, SubsNorm, [SubsT], AllowNull) ->
  lists:flatten ([Gvn, "(", lists:reverse (SubsNorm),
      "\"", escape_key (SubsT, AllowNull), "\"", ")"]);
format_gvn_internal (Gvn, SubsNorm, [S|SubsT], AllowNull) ->
  format_gvn_internal (Gvn, ["\",", escape_key (S, false), "\"" |SubsNorm],
    SubsT, AllowNull).

%% @doc Format a routine/program call command.
%% It works exactly in the same way as `format_gvn'.
%%
%% ```
%% erl> format_pgm_call ("MoveFile^MyRoutine", ["oldfile", "newfile"]).
%% "MoveFile^MyRoutine(\"oldfile\",\"newfile\")"
%% '''
format_pgm_call (Pgm, Args) ->
  format_gvn (Pgm, Args).

%% @doc Escape a single subscript key.
%% Encode the argument to be safe to use in
%% `format_gvn' and similar functions.
%% Optionally, the second `AllowNull' argument
%% allows the key to be empty, otherwise it will
%% use `?EGTM_NULLKEY' macro replacement.
escape_key ([], AllowNull) -> escape_key (undefined, AllowNull);
escape_key (undefined, false) -> ?EGTM_NULLKEY;
escape_key (undefined, true) -> [];
escape_key (Key, _) -> term2str (Key).

%% @doc Unescape a single subscript key.
%% The opposite of `escape_key'.
unescape_key (Val) -> mumps_unescape (Val).

%% @doc Escape a value to be stored in GT.M.
%% Convert any Erlang term to string that could
%% be safely stored into GT.M database.
%%
%% At the moment, it calls `term2str' in turn.
escape_val (Val) -> term2str (Val).

%% @doc Unescape a value from GT.M backend.
unescape_val (Val) -> mumps_unescape (Val).

%% @doc Convert an Erlang term to GT.M-friendly string.
%%
%% Note that if the argument is any non-scalar,
%% simple or complex term, this operation uses
%% io_lib:format which significantly slows things down!
term2str ([]) -> "";
term2str (undefined) -> "";
term2str (T) when is_atom (T) -> atom_to_list (T);
term2str (T) when is_integer (T) -> integer_to_list (T);
% NOTE: 256.99 -> 2.56990000000000009095e+02 (what is wrong!)
% Let's fall all the floats to the slow and ugly io_lib/~p...
%term2str (T) when is_float (T) -> float_to_list (T);
term2str (T) when is_list (T) -> mumps_escape (T);
term2str (T) -> term2str (lists:flatten (io_lib:format ("~p", [T]))).

%% @doc Simple quote-escaping for values passed to MUMPS.
%% Doubles all the quotes in the string to be MUMPS-friendly.
mumps_escape ([]) -> [];
mumps_escape ([$"|T]) -> [$",$"|mumps_escape (T)];
mumps_escape ([H|T]) -> [H|mumps_escape (T)].

%% @doc Simple quote-unescaping of MUMPS values.
%% Makes all the doubled quotes converted to a single
%% quotes to decode a standard MUMPS strings loaded
%% from backend.
mumps_unescape ([]) -> [];
mumps_unescape ([$",$"|T]) -> [$"|mumps_unescape (T)];
mumps_unescape ([H|T]) -> [H|mumps_unescape (T)].


%% EUnit Tests
-ifdef (TEST).
-include_lib ("eunit/include/eunit.hrl").

basic_test () ->
  Gvn = "^Test",

  ?assertEqual (Gvn,
    ?MODULE:format_gvn (Gvn, [])),
  ?assertEqual (Gvn++"(\"foo\")",
    ?MODULE:format_gvn (Gvn, ["foo"])),
  ?assertEqual (Gvn++"(\"1\",\"2\",\"1\",\"2\")",
    ?MODULE:format_gvn (Gvn, [1, 2, 1, 2])),

  UnEscVal = "He said: \"the software is called 'egtm'!\"",
  ?assertEqual (UnEscVal, ?MODULE:mumps_unescape (?MODULE:mumps_escape (UnEscVal))),

  ok.

-endif.

%% vim: fdm=syntax:fdn=3:tw=74:ts=2:syn=erlang
