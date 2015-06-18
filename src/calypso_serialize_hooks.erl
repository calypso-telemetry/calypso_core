-module(calypso_serialize_hooks).
-author("Sergey Loguntsov").

%% API
-export([
  export_hook/2, import_hook/2, delete_hooks/1
]).

import_hook(Id, Function) ->
  calypso_hooks:add(serialize, Id, Function).

export_hook(Id, Function) ->
  calypso_hooks:add(unserialize, Id, Function).

delete_hooks(Id) ->
  calypso_hooks:delete(serialize, Id),
  calypso_hooks:delete(unserialize, Id),
  ok.