%%
%% $Id: $
%%
%% Module:  egtm_worker -- description
%% Created: 05-APR-2012 20:11
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

%% @doc EGTM worker process bound directly to GT.M NIF library.
-module (egtm_worker).
-behaviour (gen_server).
-compile (nowarn_unused_function).
-compile (export_all). % XXX: remove this!

-export ([init/1,
  handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).
-export ([start_link/0, perform/2]).

-include_lib ("egtm.hrl").

-define (LIBNAME, ?MODULE).
-on_load (nif_init/0).

init (_Args) ->
  ?report_info ("Starting up ~p on node ~p... ~n", [?MODULE, node ()]),
  {ok, []}.

start_link () ->
  gen_server:start_link ({local, ?MODULE}, ?MODULE, [], []).

%% @doc Call a core-EGTM operation `Operation' with
%% arguments `Args' via this worker server process.
perform (Operation, Args) ->
  case catch (gen_server:call (?MODULE, {op, Operation, Args})) of
    {'EXIT', X} -> {error, X};
    Result      -> Result
  end.

handle_call ({op, Operation, Args} = _Request, _From, State) ->
  OpM = list_to_atom ("m_" ++ atom_to_list (Operation)),
  case catch (apply (?MODULE, OpM, Args)) of
    {'EXIT',_} -> {reply, {error, when_invoking}, State};
    Result     -> {reply, Result, State}
  end.

handle_cast (_Msg, State) ->
  {noreply, State}.

handle_info (_Info, State) ->
  {noreply, State}.

terminate (_Reason, _State) ->
  ok.

code_change (_OldVsn, State, _Extra) ->
  {ok, State}.

%% --- Internal NIF API ---------
m_get        (_)          -> not_loaded (?LINE).
m_getp       (_, _, _)    -> not_loaded (?LINE).
m_set        (_, _)       -> not_loaded (?LINE).
m_setp       (_, _, _, _) -> not_loaded (?LINE).
m_order      (_, _)       -> not_loaded (?LINE).
m_fast_order (_, _)       -> not_loaded (?LINE).
m_kill       (_)          -> not_loaded (?LINE).
m_zkill      (_)          -> not_loaded (?LINE).
m_do         (_)          -> not_loaded (?LINE).
m_call       (_)          -> not_loaded (?LINE).
m_merge      (_, _)       -> not_loaded (?LINE).
m_tstart     (_)          -> not_loaded (?LINE).
m_tcommit    ()           -> not_loaded (?LINE).
m_trollback  ()           -> not_loaded (?LINE).
m_lock       (_)          -> not_loaded (?LINE).
m_unlock     (_)          -> not_loaded (?LINE).
m_data       (_)          -> not_loaded (?LINE).
m_xecute     (_)          -> not_loaded (?LINE).

m_horo       ()           -> not_loaded (?LINE).
m_zver       ()           -> not_loaded (?LINE).
m_job        ()           -> not_loaded (?LINE).
m_iget       (_)          -> not_loaded (?LINE).


%% --- NIF initialization ---------
nif_init () ->
  SoName = egtm_env:get_priv_dir (?LIBNAME),
  ?trace ("Loading EGTM NIF: ~s", [SoName]),
  egtm_env:init (),
  case erlang:load_nif (SoName, 0) of
    %% XXX: NIF is not ready for upgrades yet :-(
    %{error, {upgrade, _}} -> % new and preferred, rather than reload
    %  ?report_warning ("EGTM NIF: upgrade not supported yet!"),
    %  ok; % tralala... everything is ok, isn't it? :-)
    %{error, {reload, _}} -> % reload is deprecated
    %  ?report_warning ("EGTM NIF: reload not supported yet!"),
    %  ok; % tralala... everything is ok, isn't it? :-)
    ok -> ok; AnotherError -> AnotherError
  end.

not_loaded (Line) ->
  exit ({not_loaded, [{module, ?MODULE}, {line, Line}]}).

%% vim: fdm=syntax:fdn=3:tw=74:ts=2:syn=erlang
