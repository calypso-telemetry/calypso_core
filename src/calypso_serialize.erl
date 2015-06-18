-module(calypso_serialize).
-author("Sergey Loguntsov").

%% API
-export([
  export/2, import/2,
  to_binary_keys/1
]).

export(Type, Object) ->
  case calypso_hooks:run_filter(serialize, { Type, Object }, fun({ok, _}) -> true; (_) -> false end) of
    [{ok, Info }] when is_map(Info) -> { ok, Info };
    List when is_list(List) -> error(many_results_of_export, [ List ]);
    [] -> error(cant_export, [ Type, Object ])
  end.

import(Type, Info) when is_map(Info) ->
  case calypso_hooks:run_filter(unserialize, { Type, Info }, fun({ok, _}) -> true; (_) -> false end) of
    [{ok, Object}] -> { ok, Object };
    List when is_list(List) -> error(many_results_of_import, [ List ]);
    [] -> error(cant_import, [ Type, Info ])
  end.

to_binary_keys(Info) when is_map(Info) ->
  maps:fold(fun
    (Key, Value, Acc) when is_atom(Key) -> maps:put(atom_to_binary(Key, utf8), Value, Acc);
    (Key, Value, Acc) when is_binary(Key) -> maps:put(Key, Value, Acc)
  end, #{}, Info).

