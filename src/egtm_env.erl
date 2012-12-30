%%
%% $Id: $
%%
%% Module:  egtm_env -- description
%% Created: 30-DEC-2012 03:56
%% Author:  tmr
%%

-module (egtm_env).
-export ([init/0, get_priv_dir/1, syscmd/1, syscmd/2]).

-include_lib ("egtm.hrl").

init () ->
  RawEnv = syscmd (get_cmd (listenv)),
  process_env (binary:split (list_to_binary (RawEnv),
    ?EGTM_ENV_DELIM_REC)),
  os:putenv ("GTMCI", os:getenv ("EGTM_CALLINTAB")).

get_cmd (listenv) -> get_priv_dir ("listenv").

%% XXX: May be used by egtm_admin too?
get_priv_dir (Name) ->
  case code:priv_dir (?EGTM_APPNAME) of
    {error, bad_name} ->
      case filelib:is_dir (filename:join (["..", priv])) of
        true -> filename:join (["..", priv, Name]);
        _    -> filename:join ([priv, Name])
      end;
    Dir -> filename:join (Dir, Name)
  end.

process_env ([]) -> ok;
process_env ([<<>>]) -> ok;
process_env ([X]) ->
  process_env (binary:split (X, ?EGTM_ENV_DELIM_REC));
process_env ([H|T]) ->
  [K|[V]] = binary:split (H, ?EGTM_ENV_DELIM_KV),
  os:putenv (binary_to_list (K), binary_to_list (V)),
  process_env (T).

%% XXX: To be moved somewhere else
syscmd (Cmd, Args) -> syscmd (string:join ([Cmd|Args], " ")).
syscmd (Cmd) ->
  Port = erlang:open_port ({spawn, Cmd}, [exit_status]),
  loop (Port, [], 5000).

loop (Port, Data, Timeout) ->
  receive
    {Port, {data, NewData}} ->
      loop (Port, Data++NewData, Timeout);
    {Port, {exit_status, 0}} ->
      Data;
    {Port, {exit_status, S}} ->
      throw ({commandfailed, S})
  after Timeout ->
    throw (timeout)
  end.

%% vim: fdm=syntax:fdn=3:tw=74:ts=2:syn=erlang
