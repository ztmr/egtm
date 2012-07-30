%%
%% $Id: $
%%
%% Module:  egtm_pool_sup -- description
%% Created: 04-JUN-2012 00:14
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

%% @doc EGTM worker pool supervisor process.
-module (egtm_pool_sup).
-behaviour (supervisor).

%% API
-export ([start_link/0]).

%% Supervisor callbacks
-export ([init/1]).

start_link () ->
  supervisor:start_link ({local, ?MODULE}, ?MODULE, []).

init ([]) ->
  Child = {egtm_pool, {egtm_pool, start_link, []},
           permanent, 2000, worker, [egtm_pool]},
  {ok, {{one_for_one, 1, 1}, [Child]}}.

%% vim: fdm=syntax:fdn=3:tw=74:ts=2:syn=erlang
