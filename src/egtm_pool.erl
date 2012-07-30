%%
%% $Id: $
%%
%% Module:  egtm_pool -- description
%% Created: 06-APR-2012 22:40
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

%% @doc EGTM worker pool manager process.
%%
%% In principle, it is similar to standard Erlang `pool' module,
%% and since we use GT.M via NIF interface, we have to run
%% each worker in a separate Erlang process to make it some sense.
%%
%% This is done via `slave' module that "forks" off slave
%% nodes based on configuration `[egtm, workers, nodes]', the
%% array of nodenames to start.
%%
%% So this pool forms a small Erlang distribution cluster
%% restricted to run on a single host with the same GT.M
%% database.
%%
%% For more advanced configurations with multiple hosts,
%% replication, intelliroute, and other advanced features,
%% take a look at EGTM/Cluster product.
%%
%% Keep in mind that since pool uses EPMD-based RPC between
%% slave ErlVM nodes, it is always slower than single-worker
%% configuration. Basic test shows about 10-time slowdown.
%% That's the reason why egtm_pool is disabled by default
%% and to use it, you have to compile EGTM with `EGTM_POOL_ENABLED'
%% macro defined.
%%
%% Once we're compiled with `EGTM_POOL_ENABLED', you can
%% configure pool by setting `[egtm, mode]' to `pool'.
-module (egtm_pool).
-behaviour (gen_server).

%-export ([start/0, status/0, perform/2, restart/0, stop/0]).
-export ([init/1,
  handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).
-export ([start_link/0, perform/2]).
-compile (export_all).

-include ("egtm.hrl").

init (_Args) ->
  ?report_info ("Starting up ~p...", [?MODULE]),
  {ok, [ {N, node_bring_up (N)} || N <- get_nodes ('configured') ]}.

start_link () ->
  gen_server:start_link ({local, ?MODULE}, ?MODULE, [], []).

%% @doc Call a core-EGTM operation `Operation' with
%% arguments `Args' via one of `egtm_worker' processes
%% in this pool.
perform (Operation, Args) -> perform (Operation, Args, 2).
perform (_Operation, _Args, 0) -> {timeout, retry_limit_reached};
perform (Operation, Args, _Retries) ->
  case catch (gen_server:call (?MODULE, {op, Operation, Args})) of
    {'EXIT', X}  -> {error, X};
      %case X of
      %  {timeout, _} ->
      %    ?report_error ("Request timeout, trying again..."),
      %    timer:sleep (500),
      %    perform (Operation, Args, Retries-1);
      %  X -> ?report_error (X), {error, X}
      %end;
    Result -> Result
  end.

handle_call ({op, Op, Args} = _Request, _From, State) ->
  case catch rpc:call (select_node (), egtm, perform, [Op, Args]) of
    {'EXIT',_} -> {reply, {error, when_invoking}, State};
    Result     -> {reply, Result, State}
  end.

handle_cast (_Msg, State) ->
  {noreply, State}.

%% @doc Some of our nodes has died, let's start it back!
handle_info ({nodedown, Node}, State) ->
  ?report_warning ("Trying to restart just died node " ++
                   atom_to_list (Node) ++ "..."),
  node_bring_up (Node),
  {noreply, State};
handle_info (_Info, State) ->
  {noreply, State}.

terminate (_Reason, _State) ->
  multicast (get_nodes ('configured'), erlang, halt, []),
  timer:sleep (2000),
  [ net_adm:ping (N) || N <- get_nodes ('configured') ],
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

multicast ([], _M, _F, _A) -> ok;
multicast ([Node|Nodes], M, F, A) ->
  rpc:cast (Node, M, F, A),
  multicast (Nodes, M, F, A).

get_nodes ('configured') ->
  [ list_to_atom (atom_to_list (N)++"@"++net_adm:localhost ())
    || N <- egtm_config:param ([egtm,workers,nodes]) ];
get_nodes ('active') ->
  [ node () | nodes () ];
get_nodes ('broken') ->
  get_nodes ('configured') -- get_nodes ('active').

status () ->
  case nodes () of
    []    -> {single};
    Nodes -> {pooled,
               {nodes, {online, Nodes},
               {broken, get_nodes ('broken')}}}
  end.

node_bring_up (N) ->
  case net_adm:ping (N) of
    pang ->
      %Cmd = lists:flatten (
      %  io_lib:format ("erl -pa ebin -sname ~s -detached", [N])),
      %io:format ("Running: ~s~n", [Cmd]),
      %os:cmd (Cmd),
      [Node, Host] = string:tokens (atom_to_list (N), "@"),
      %io:format ("Boot: ~s~n", [N]),
      Args = "-pa ebin -pa deps/*/ebin -run egtm start",
      %% XXX: what about node-level supervision?
      slave:start_link (Host, Node, Args),
      timer:sleep (2000),
      net_kernel:connect (N),
      erlang:monitor_node (N, true);
    _ ->
      already
  end.

%% RPC to Remote node is 10 times slower than on local node -- even if the
%% remote node is running on the same host!! :-((
select_node () ->
  case nodes () of
    [] -> node ();
    Nodes ->
      Count = length (Nodes),
      %% XXX: well, the uniform distribution is very naive solution;
      %% we should make some {node,pool,cluster}-wide counters
      %% to monitor local EGTM nodes as well as remote cluster peers.
      %% This may be also published via SNMP for monitoring purposes.
      %% In future, we can use decision logic based on Folsom Metrics
      %% via egtm_metrics module.
      lists:nth (random:uniform (Count), Nodes)
  end.

%% vim: fdm=syntax:fdn=3:tw=74:ts=2:syn=erlang
