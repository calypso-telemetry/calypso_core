-module(calypso_db_hooks).
-author("Sergey Loguntsov").

%% API
-export([
  create_hook/2, get_hook/2, update_hook/2, destroy_hook/2,
  get_telemetry_hook/2, save_telemetry_hook/2,
  get_report_hook/2,
  import_hook/2, export_hook/2,
  delete_hooks/1
]).

create_hook(Id, Function) ->
  calypso_hooks:add(db_create, Id, Function).

get_hook(Id, Function) ->
  calypso_hooks:add(db_get, Id, Function).

update_hook(Id, Function) ->
  calypso_hooks:add(db_update, Id, Function).

destroy_hook(Id, Function) ->
  calypso_hooks:add(db_destroy, Id, Function).

get_telemetry_hook(Id, Function) ->
  calypso_hooks:add(db_get_telemetry, Id, Function).

save_telemetry_hook(Id, Function) ->
  calypso_hooks:add(db_save_telemetry, Id, Function).

get_report_hook(Id, Function) ->
  calypso_hooks:add(db_get_report, Id, Function).

export_hook(Id, Function) ->
  calypso_hooks:add(db_export, Id, Function).

import_hook(Id, Function) ->
  calypso_hooks:add(db_import, Id, Function).

delete_hooks(Id) ->
  calypso_hooks:delete(db_create, Id),
  calypso_hooks:delete(db_get, Id),
  calypso_hooks:delete(db_update, Id),
  calypso_hooks:delete(db_destroy, Id),
  calypso_hooks:delete(db_get_telemetry, Id),
  calypso_hooks:delete(db_save_telemetry, Id),
  calypso_hooks:delete(db_get_report, Id),
  ok.
