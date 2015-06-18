-module(calypso_telemetry_hooks).
-author("Sergey Loguntsov").

-include("cl_telemetry.hrl").

%% API
-export([
  telemetry_fire/2, telemetry_hook/2, delete_hooks/1
]).

telemetry_fire(Object, Telemetry = #telemetry{}) ->
  calypso_hooks:run(set_telemetry, { Object, Telemetry }),
  ok.

telemetry_hook(Id, Function) ->
  calypso_hooks:add(set_telemetry, Id, Function).

delete_hooks(Id) ->
  calypso_hooks:delete(set_telemetry, Id),
  ok.
