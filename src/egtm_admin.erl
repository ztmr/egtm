%%
%% $Id: $
%%
%% Module:  egtm_admin -- description
%% Created: 28-APR-2012 20:10
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

%% @doc EGTM Administration Tools.
-module (egtm_admin).
-export ([initdb/0, backup/1, restore/1]).

-include ("egtm.hrl").

%% @doc Initialize database using `$PROJECT/priv/initdb'
initdb () ->
  output (run_util ("initdb")).

%% @doc Make a ZWR-backup using MUPIP EXTRACT
backup (File) ->
  output (run_util ("mupip", ["extract", File])).

%% @doc Restore a ZWR-backup file using MUPIP LOAD
restore (File) ->
  output (run_util ("mupip", ["load", File])).

output (String) ->
  ?report_info (String), String.

run_util (Name) -> run_util (Name, []).
run_util (Name, Args) ->
  Cmd = [get_util (Name), " ", string:join (Args, " ")],
  ?report_info ("Running OS command: ~s", [lists:flatten (Cmd)]),
  os:cmd (lists:flatten (Cmd)).

get_util (Name) ->
  Path = case code:priv_dir (?EGTM_APPNAME) of
    {error, bad_name} ->
      case filelib:is_dir (filename:join (["..", priv])) of
        true -> filename:join (["..", priv, Name]);
        _    -> filename:join ([priv, Name])
      end;
    Dir -> filename:join (Dir, Name)
  end,
  case Path of
    [$/|P] -> "/"++P;
    P      -> "./"++P
  end.

%% vim: fdm=syntax:fdn=3:tw=74:ts=2:syn=erlang
