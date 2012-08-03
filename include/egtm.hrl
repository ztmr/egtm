%%
%% $Id: $
%%
%% Module:  egtm -- description
%% Created: 08-MAY-2012 17:14
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

-ifndef (EGTM_HRL).
-define (EGTM_HRL, true).

-type order_direction () :: forward | backward. %% $Order direction.
-type global_name () :: string (). %% MUMPS Global Variable Name.
-type subscripts () :: list (). %% MUMPS Subscript Index Path.
-type tp_options () :: string (). %% MUMPS transaction processing options.
-type program_name () :: string (). %% MUMPS entryref `Label^MyRoutine'

-ifndef (EGTM_APPNAME).
-define (EGTM_APPNAME, egtm).
-endif.

-ifndef (EGTM_NULLKEY).
-define (EGTM_NULLKEY, "#null").
-endif.

-ifndef (EGTM_LONGSTRING_BLOCKSIZE).
-define (EGTM_LONGSTRING_BLOCKSIZE, 4000).
-endif.

-define (str (V), egtm_util:stringify (V)).

-ifdef (EGTM_TRACE).
-define (trace (Name), ?trace (Name, [])).
-define (trace (Name, Args),
  lager:debug ("EGTM CallTrace: ~s:~s ~s",
    [?MODULE, ?str (Name), ?str (Args)])).
-define (trace_code (Code), Code).
-else.
-define (trace (Name), ok).
-define (trace (Name, Args), ok).
-define (trace_code (Code), ok).
-endif.

-define (report_error (Error),
  lager:error ("EGTM Common Error: ~s: ~s",
    [?MODULE, ?str (Error)])).

-define (report_warning (Warn),
  lager:warning ("EGTM Common Warning: ~s: ~s",
    [?MODULE, ?str (Warn)])).

-define (report_info (Info), ?report_info (Info, [])).
-define (report_info (Info, Args),
  lager:info (Info, Args)).

-ifdef (EGTM_METRICS).
-define (metrics (Name, Fun),
  egtm_metrics:submit (Name, Fun)).
-else.
-define (metrics (Name, Fun), Fun ()).
-endif.

-endif. % EGTM_HRL

%% vim: fdm=syntax:fdn=3:tw=74:ts=2:syn=erlang
