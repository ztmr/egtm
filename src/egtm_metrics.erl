%%
%% $Id: $
%%
%% Module:  egtm_metrics -- description
%% Created: 22-APR-2012 23:44
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

%% @doc EGTM Code Metering Tools.
-module (egtm_metrics).
-export ([submit/2]).

-include_lib ("egtm.hrl").

ensure_running () ->
  case application:start (folsom) of
    ok -> ok;
    {error, {already_started, _}} -> ok;
    {error, _} ->
      ?report_warning ("Unable to start Folsom, the metrics server!"),
      ok   %% XXX: handle this!
  end.

%% @doc Submit a code to be metered.
%% This function will execute function `Fun' while
%% measuring execution time and save that time into
%% a specified namespace `Nsp' within Folsom Metrics
%% toolset.
%%
%% `Nsp' may look like:
%% <ul>
%% <li>`egtm.operation.get', `.set', `.order', ...</li>
%% <li>`egtm.cluster.operation.get', `.set', `.order', ...</li>
%% </ul>
%%
%% At the moment, we measure only execution time
%% (histogram) and number of executions (counter).
%%
%% To simplify calling `egtm_metrics:submit()', there is
%% also `?metrics ()' macro available.
%%
%% Example: ```
%%   ?metrics ("myapp.call.myfun", fun () -> myfun (1, 2, 3) end).
%% '''
submit (Nsp, Fun) when is_atom (Nsp) and is_function (Fun) ->
  submit (atom_to_list (Nsp), Fun);
submit (Nsp, Fun) when is_list (Nsp) and is_function (Fun) ->
  case egtm_config:param ([egtm_metrics, enabled]) of
    true ->
      ensure_running (),
      {Time, Result} = timer:tc (fun () -> Fun () end),
      NspH = Nsp ++ ".histogram",
      NspC = Nsp ++ ".counter",
      folsom_metrics:new_histogram (NspH),
      folsom_metrics:notify ({NspH, Time}),
      folsom_metrics:new_counter (NspC),
      folsom_metrics_counter:inc (NspC),
      Result;
    _ ->
      Fun ()
  end.

%% XXX: analyze metrics data
% folsom_metrics:get_histogram_statistics (Nsp)
% folsom_metrics:get_metric_value (Nsp)

%% vim: fdm=syntax:fdn=3:tw=74:ts=2:syn=erlang
