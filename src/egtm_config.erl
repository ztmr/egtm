%%
%% $Id: $
%%
%% Module:  egtm_config -- description
%% Created: 08-APR-2012 00:43
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

%% @doc EGTM Configuration Manager.
%% EGTM configuration is loaded from file that
%% may look like this one:
%%
%% ```
%% %% egtm core setup
%% {egtm, [
%% 
%%   %% Defaults
%%   {defaults, [
%% 
%%     %% $Piece default delimiter
%%     {piece_delim, "|"}
%%   ]},
%% 
%%   %% Mode of operation
%%   %%   single (= use NIF directly),
%%   %%   pool (= use multiple slave ErlVMs)
%%   %% NOTE: pooling is 10times slower than 'single'
%%   %% and is also disabled by default. To enable it,
%%   %% you need to define EGTM_POOL_ENABLED macro
%%   {mode, single},
%% 
%%   %% Workers are slave ErlVMs with GT.M call-in NIF
%%   {workers, [
%% 
%%     %% Slave nodes to be autostarted
%%     {nodes, [egtm1, egtm2, egtm3, egtm4]}
%%   ]},
%% 
%%   %% Functions that are for some (security) reason denied
%%   %{deny, [kill, do, call, merge, xecute]}
%%   {deny, []}
%%   %,
%%   %% String encoder/decoder functions
%%   %{string_conversion, [
%%   %  {encode, {egtm_string, erl2utf} },
%%   %  {decode, {egtm_string, utf2erl} } ]}
%% ]}.
%% 
%% %% egtm metrics: histograms and counters
%% %% NOTE: if enabled, all egtm-core operations are slower!
%% {egtm_metrics, [{enabled, false}]}.
%% '''
-module (egtm_config).
-export ([param/1]).

-include_lib ("egtm.hrl").

config_name () -> "egtm.conf". %% to be optionally overriden
module_name () -> ?EGTM_APPNAME. %% to be optionally overriden

config_path () ->
  ConfName = config_name (),
  case code:priv_dir (module_name ()) of
    {error,_} -> filename:join (["priv", ConfName]);
    Path      -> filename:join ([Path, ConfName])
  end.

%% NOTE: This is to be inherited and overriden
%% for egtm_cluster_config and iodb_config modules.
defaults () ->
  [{egtm,
       [{defaults,[{piece_delim,"|"}]},
        {mode, single},
        {workers,[{nodes,[egtm1,egtm2,egtm3,egtm4]}]},
        {deny,[kill,do,call,merge,xecute]}]},
   {egtm_metrics, [{enabled, false}]}].

%% @doc Get a config value from `priv/egtm.conf'.
%% Example: ```egtm_config:param ([egtm, workers, nodes])'''
%% ...will return `[a,b,c,d]' from config file like this:
%% ```{egtm, [{workers, [{nodes, [a,b,c,d]}]}]}'''
-spec param (Path::list ()) -> Result::any ().
param (Path) ->
  param (Path, param ()).
param (Path, Conf) ->
  case deepprops:get (Path, Conf) of
    undefined -> deepprops:get (Path, defaults ());
    Value     -> Value
  end.
param () ->
  case application:get_env (module_name (), config) of
    undefined -> 
      case ct_config_plain:read_config (config_path ()) of
        {ok, Conf} ->
          application:set_env (module_name (), config, Conf),
          Conf;
        {error, Error} ->
          io:format ("ConfigError: ~p~n", [Error]),
          application:set_env (module_name (), config, {}),
          {};
        _ ->
          application:set_env (module_name (), config, {}),
          {}
      end;
    {ok, Conf} -> Conf
  end.

%% vim: fdm=syntax:fdn=3:tw=74:ts=2:syn=erlang
