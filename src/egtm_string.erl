%%
%% $Id: $
%%
%% Module:  egtm_string -- description
%% Created: 28-APR-2012 20:02
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

%% @doc General purpose string encoding library.
%% This library is used internally for egtm string encoding
%% from/to egtm's "native" encoding (depends on GT.M settings).
%% Such a transformation is driven by `priv/egtm.conf' config
%% of `[egtm, string_conversion, encode]' and
%% `[egtm, string_conversion, decode]'.
%% 
%% Using this library, it is also very easy to encrypt/decrypt
%% strings before saving/loading to/from database.
-module (egtm_string).
-export ([encode/1, decode/1]).
-export ([erl2utf/1, utf2erl/1]).

%% @doc Encode string using function referred in config.
encode (X) when is_list (X) ->
  encode (egtm_config:param([egtm, string_conversion, encode]), X);
encode (X) -> X.
encode ({Mod, Fun}, X) ->
  apply (Mod, Fun, [X]);
encode (undefined, X) -> X.

%% @doc Decode string using function referred in config.
decode (X) when is_list (X) ->
  decode (egtm_config:param([egtm, string_conversion, decode]), X);
decode (X) -> X.
decode ({Mod, Fun}, X) ->
  apply (Mod, Fun, [X]);
decode (undefined, X) -> X.

%% @doc Convertor from Unicode to Erlang's native encoding.
%% Erlang's native encoding is "encoding-agnostic"
%% Latin1-friendly list of bytes.
utf2erl (S) ->
  Fun = fun () -> binary_to_list (unicode:characters_to_binary (S)) end,
  case catch (Fun ()) of {'EXIT', _} -> S; R -> R end.

%% @doc Convertor from Erlang's native encoding to Unicode.
%% Erlang's native encoding is "encoding-agnostic"
%% Latin1-friendly list of bytes.
erl2utf (S) ->
  Fun = fun () -> unicode:characters_to_list (list_to_binary (S)) end,
  case catch (Fun ()) of {'EXIT', _} -> S; R -> R end.

%% vim: fdm=syntax:fdn=3:tw=74:ts=2:syn=erlang
