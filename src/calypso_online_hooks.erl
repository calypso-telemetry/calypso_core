-module(calypso_online_hooks).
-author("begemot").

%% API
-export([
  fire_online/1, fire_online/2, fire_offline/1,
  online_hook/2, offline_hook/2, delete_hooks/1
]).

fire_online(Object) ->
  fire_online(Object, []).

fire_online(Object, Registers) ->
  calypso_registrar:register(Registers),
  calypso_hooks:run(object_online, Object).

fire_offline(Object) ->
  calypso_hooks:run(object_offline, Object).

online_hook(Id, Function) ->
  calypso_hooks:add(object_online, Id, Function).

offline_hook(Id, Function) ->
  calypso_hooks:add(object_offline, Id, Function).

delete_hooks(Id) ->
  calypso_hooks:delete(object_online, Id),
  calypso_hooks:delete(object_offline, Id),
  ok.

