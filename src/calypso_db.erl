-module(calypso_db).
-author("Sergey Loguntsov").

-include("cl_telemetry.hrl").

%% API
-export([
  get/2, create/2, destroy/2, update/2,
  get_telemetry/3, get_telemetry/4, save_telemetry/2,
  get_report/3
]).

get(Type, Id) ->
  case calypso_hooks:run_before_result(db_get, {Type, Id}) of
    { ok, _ModuleId, Object } -> { ok, Object };
    { value, _ModuleId, Value } -> Value;
    undefined -> undefined
  end.

update(Type, Object) ->
  Result = calypso_hooks:run_map(db_update, { Type, Object }),
  case [ ok || { _, ok } <- Result ] of
    [ok|_] -> ok;
    [] -> { error, not_updated, Result }
  end.

create(Type, Object) ->
  Result = calypso_hooks:run_map(db_create, { Type, Object }),
  case [ Item || { _, {ok, _ } = Item} <- Result ] of
    [{ok, NewId}|_] -> { ok, NewId };
    [] -> { error, not_created, Result }
  end.

destroy(Type, Object) ->
  case calypso_hooks:run_boolean_any(db_destroy, { Type, Object }, fun({ok, _}) -> true;(ok) -> true;(_) -> false end) of
    true -> ok;
    false -> { error, not_deleted }
  end.

get_telemetry(Type, Object, Period) ->
  get_telemetry(Type, Object, Period, #{}).
get_telemetry(Type, Object, Period, Options) ->
  case calypso_hooks:run_before_result(db_get_telemetry, { Type, Object, Period, Options }) of
    { ok, _Id, Telemetry } ->
      { ok, Telemetry };
    undefined -> undefined
  end.

save_telemetry(Object, Telemetry = #telemetry{}) ->
  case calypso_hooks:run_boolean_any(db_save_telemetry, { Object, Telemetry }, fun({ok, _}) -> true;(ok) -> true;(_) -> false end) of
    true -> ok;
    false -> error(doesnt_save_telemetry)
  end.

get_report(ReportName, Period, Options) ->
  case calypso_hooks:run_before_result(db_get_report, { ReportName, Period, Options }) of
    { ok, _Id, Report } ->
      { ok, Report };
    undefined -> undefined
  end.


